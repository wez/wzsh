use crate::builtins::{lookup_builtin, BuiltinCommand};
use crate::exitstatus::{ExitStatus, WaitableExitStatus};
use crate::filedescriptor::FileDescriptor;
use crate::job::{make_foreground_process_group, make_own_process_group, Job, JobList};
use crate::parse::{
    Command as ShellCommand, CommandType, FileRedirection, Redirection, SimpleCommand,
};
use crate::ShellExpander;
use failure::{bail, ensure, Fail, Fallible};
use shlex::string::ShellString;
use shlex::{Aliases, Environment, Expander, Token, TokenKind};
use std::cell::{RefCell, RefMut};
use std::process::Command;
use std::rc::Rc;
use std::sync::Arc;

pub enum RunnableCommand {
    ChildProcess(std::process::Command),
    Builtin(BuiltinCommand),
}

#[derive(Clone)]
pub struct ExecutionEnvironment {
    env: Rc<RefCell<Environment>>,
    aliases: Rc<RefCell<Aliases>>,
    jobs: Arc<JobList>,

    stdin: Rc<RefCell<FileDescriptor>>,
    stdout: Rc<RefCell<FileDescriptor>>,
    stderr: Rc<RefCell<FileDescriptor>>,
}

impl ExecutionEnvironment {
    pub fn new() -> Fallible<Self> {
        Ok(Self {
            env: Rc::new(RefCell::new(Environment::new())),
            jobs: Arc::new(JobList::default()),
            aliases: Rc::new(RefCell::new(Aliases::new())),
            stdin: Rc::new(RefCell::new(FileDescriptor::dup(std::io::stdin())?)),
            stdout: Rc::new(RefCell::new(FileDescriptor::dup(std::io::stdout())?)),
            stderr: Rc::new(RefCell::new(FileDescriptor::dup(std::io::stderr())?)),
        })
    }

    pub fn job_list(&self) -> Arc<JobList> {
        Arc::clone(&self.jobs)
    }

    pub fn stdin(&self) -> RefMut<std::io::Read> {
        self.stdin.borrow_mut()
    }
    pub fn stdout(&self) -> RefMut<std::io::Write> {
        self.stdout.borrow_mut()
    }
    pub fn stderr(&self) -> RefMut<std::io::Write> {
        self.stderr.borrow_mut()
    }

    pub fn set_stdin(&mut self, fd: FileDescriptor) {
        self.stdin = Rc::new(RefCell::new(fd));
    }

    pub fn set_stdout(&mut self, fd: FileDescriptor) {
        self.stdout = Rc::new(RefCell::new(fd));
    }

    fn build_command(
        &mut self,
        cmd: &SimpleCommand,
        expander: &ShellExpander,
        asynchronous: bool,
    ) -> Fallible<Option<RunnableCommand>> {
        let argv = cmd.expand_argv(&mut self.env.borrow_mut(), expander, &self.aliases.borrow())?;
        if argv.is_empty() {
            Ok(None)
        } else if let Some(builtin) = lookup_builtin(&argv[0]) {
            Ok(Some(RunnableCommand::Builtin(BuiltinCommand::new(
                builtin, argv,
            ))))
        } else {
            let mut child_cmd = Command::new(&argv[0]);
            for arg in argv.iter().skip(1) {
                child_cmd.arg(arg);
            }
            child_cmd.env_clear();
            child_cmd.envs(self.env.borrow().iter());

            child_cmd.stdin(self.stdin.borrow_mut().as_stdio()?);
            child_cmd.stdout(self.stdout.borrow_mut().as_stdio()?);
            child_cmd.stderr(self.stderr.borrow_mut().as_stdio()?);

            #[cfg(unix)]
            unsafe {
                use std::os::unix::process::CommandExt;
                child_cmd.pre_exec(move || {
                    let pid = libc::getpid();
                    if asynchronous {
                        make_own_process_group(pid);
                    } else {
                        make_foreground_process_group(pid);
                    }
                    for s in &[
                        libc::SIGINT,
                        libc::SIGQUIT,
                        libc::SIGTSTP,
                        libc::SIGTTIN,
                        libc::SIGTTOU,
                        libc::SIGCHLD,
                    ] {
                        libc::signal(*s, libc::SIG_DFL);
                    }

                    Ok(())
                });
            }

            Ok(Some(RunnableCommand::ChildProcess(child_cmd)))
        }
    }

    fn file_from_redir(
        &mut self,
        redir: &FileRedirection,
        expander: &ShellExpander,
    ) -> Fallible<std::fs::File> {
        ensure!(
            redir.fd_number < 3,
            "only stdin, stdout, stderr currently support for redirection"
        );
        let file_name = match &redir.file_name.kind {
            TokenKind::Word(word) => {
                let word = ShellString::from(word.as_str());
                let file = expander.expand_word(&word, &mut self.env.borrow_mut())?;
                ensure!(
                    file.len() == 1,
                    "{:?} expanded to {:?}, expected just a single item",
                    redir.file_name,
                    file
                );
                file.into_iter().next().unwrap()
            }
            _ => bail!("file_name is not a word token"),
        };
        let mut options = std::fs::OpenOptions::new();
        options
            .read(redir.input)
            .write(redir.output)
            .append(redir.append)
            .truncate(redir.output && !redir.append)
            // TODO: if a noclobber option is set, and !redir.clobber,
            // then we should look at .create_new() instead
            .create(redir.output || redir.clobber);
        match options.open(file_name.as_ref()) {
            Ok(file) => Ok(file),
            Err(e) => {
                return Err(e
                    .context(format!("opening '{}' using {:#?}", file_name, options))
                    .into());
            }
        }
    }

    pub fn apply_redirections_to_env(
        &mut self,
        cmd: &SimpleCommand,
        expander: &ShellExpander,
    ) -> Fallible<()> {
        for redir in &cmd.redirections {
            match redir {
                Redirection::File(redir) => {
                    let file = self.file_from_redir(redir, expander)?;
                    let fd = Rc::new(RefCell::new(FileDescriptor::dup(file)?));
                    match redir.fd_number {
                        0 => self.stdin = fd,
                        1 => self.stdout = fd,
                        2 => self.stderr = fd,
                        _ => bail!("unsupported fd number"),
                    };
                }
                Redirection::Fd(_) => {
                    bail!("fd redirection not hooked up to eval");
                }
            }
        }
        Ok(())
    }

    pub fn apply_redirections_to_cmd(
        &mut self,
        child_cmd: &mut std::process::Command,
        cmd: &SimpleCommand,
        expander: &ShellExpander,
    ) -> Fallible<()> {
        for redir in &cmd.redirections {
            match redir {
                Redirection::File(redir) => {
                    let file = self.file_from_redir(redir, expander)?;
                    match redir.fd_number {
                        0 => child_cmd.stdin(file),
                        1 => child_cmd.stdout(file),
                        2 => child_cmd.stderr(file),
                        _ => bail!("unsupported fd number"),
                    };
                }
                Redirection::Fd(_) => {
                    bail!("fd redirection not hooked up to eval");
                }
            }
        }
        Ok(())
    }

    fn apply_assignments_to_cmd(
        &mut self,
        expander: &ShellExpander,
        assignments: &Vec<Token>,
        child_cmd: &mut std::process::Command,
    ) -> Fallible<()> {
        for t in assignments {
            if let Some((key, value)) = t.kind.parse_assignment_word() {
                let fields = expander.expand_word(&value.into(), &mut self.env.borrow_mut())?;
                ensure!(
                    fields.len() == 1,
                    "variable expansion produced {:?}, expected a single field"
                );
                child_cmd.env(key, &fields[0]);
            }
        }
        Ok(())
    }
    fn apply_assignments_to_env(
        &mut self,
        expander: &ShellExpander,
        assignments: &Vec<Token>,
    ) -> Fallible<()> {
        for t in assignments {
            if let Some((key, value)) = t.kind.parse_assignment_word() {
                let fields = expander.expand_word(&value.into(), &mut self.env.borrow_mut())?;
                ensure!(
                    fields.len() == 1,
                    "variable expansion produced {:?}, expected a single field"
                );
                self.env.borrow_mut().set(key, &fields[0]);
            }
        }
        Ok(())
    }

    pub fn eval(
        &mut self,
        expander: &ShellExpander,
        command: &ShellCommand,
        job: Option<Job>,
    ) -> Fallible<Job> {
        let mut job = Job::unwrap_or_create(job, command);
        match &command.command {
            CommandType::SimpleCommand(cmd) => {
                match self.build_command(&cmd, expander, command.asynchronous)? {
                    Some(RunnableCommand::ChildProcess(mut child_cmd)) => {
                        self.apply_redirections_to_cmd(&mut child_cmd, &cmd, expander)?;
                        self.apply_assignments_to_cmd(expander, &cmd.assignments, &mut child_cmd)?;
                        let child = child_cmd.spawn()?;

                        // To avoid a race condition with starting up the child, we
                        // need to also munge the process group assignment here in
                        // the parent.  Note that the loser of the race will experience
                        // errors in attempting this block, so we willfully ignore
                        // the return values here: we cannot do anything about them.
                        #[cfg(unix)]
                        {
                            let pid = child.id() as i32;
                            if command.asynchronous {
                                make_own_process_group(pid);
                            } else {
                                make_foreground_process_group(pid);
                            }
                        }

                        job.add(WaitableExitStatus::with_child(child), command.asynchronous)?;
                        Ok(self.jobs.add(job))
                    }
                    Some(RunnableCommand::Builtin(builtin)) => {
                        let mut env = self.clone();
                        env.apply_redirections_to_env(&cmd, expander)?;
                        env.apply_assignments_to_env(expander, &cmd.assignments)?;
                        job.add(WaitableExitStatus::Done(builtin.run(&env)?), false)?;
                        Ok(self.jobs.add(job))
                    }
                    None => {
                        self.apply_redirections_to_env(&cmd, expander)?;
                        self.apply_assignments_to_env(expander, &cmd.assignments)?;
                        job.add(WaitableExitStatus::Done(ExitStatus::new_ok()), false)?;
                        Ok(self.jobs.add(job))
                    }
                }
            }
            CommandType::BraceGroup(list) => {
                for cmd in &list.commands {
                    self.eval(expander, &cmd, Some(job.clone()))?;
                }
                Ok(self.jobs.add(job))
            }
            CommandType::Subshell(list) => {
                // Posix wants the subshell to be a forked child,
                // but we don't do that in the interests of portability
                // to windows.  Instead we clone the current execution
                // environment and use that to run the list.
                // We'll probably need to do something smarter
                // here to manage signals/process groups on posix systems,
                // and for async subshells, may want to consider using
                // a thread to execute it.
                let mut sub_env = self.clone();
                for cmd in &list.commands {
                    sub_env.eval(expander, &cmd, Some(job.clone()))?;
                }
                Ok(self.jobs.add(job))
            }
            CommandType::Pipeline(pipeline) => {
                let mut envs = vec![];
                for _ in 0..pipeline.commands.len() {
                    envs.push(self.clone());
                }

                for i in 0..pipeline.commands.len() - 1 {
                    let pipe = FileDescriptor::pipe()?;

                    envs[i].set_stdout(pipe.write);
                    envs[i + 1].set_stdin(pipe.read);
                }

                // We need to release the pipe handles as we
                // work through the pipe sequence, and the easiest
                // way to do that is to reverse this sequence and
                // pop the envs off as we walk forwards through the
                // pipeline.
                envs.reverse();

                for cmd in &pipeline.commands {
                    let mut env = envs.pop().unwrap();
                    env.eval(expander, cmd, Some(job.clone()))?;
                    // We want to drop the env so that its ref to the
                    // pipes is released, causing the pipes to cascade
                    // shut as we move along.
                    drop(env);
                }

                let mut job = self.jobs.add(job);

                // FIXME: is command.asynchronous shouldn't wait.
                // But also: what about inverted-ness if it is async?
                let status = job.wait()?;
                let status = if pipeline.inverted {
                    status.invert()
                } else {
                    status
                };
                job.add(WaitableExitStatus::Done(status), false)?;
                Ok(job)
            }
            _ => bail!("eval doesn't know about {:#?}", command),
        }
    }
}
