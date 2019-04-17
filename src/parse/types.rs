use failure::{bail, Fallible};
use shlex::string::ShellString;
use shlex::{Aliases, Environment, Expander, Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Command {
    pub asynchronous: bool,
    pub command: CommandType,
    pub redirects: Option<RedirectList>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RedirectList {
    pub redirections: Vec<Redirection>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommandType {
    Pipeline(Pipeline),
    SimpleCommand(SimpleCommand),
    BraceGroup(CompoundList),
    Subshell(CompoundList),
    ForEach(ForEach),
    If(If),
    UntilLoop(UntilLoop),
    WhileLoop(WhileLoop),
    // TODO: Case
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pipeline {
    /// true if the pipeline starts with a bang
    pub inverted: bool,
    pub commands: Vec<Command>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundList {
    pub commands: Vec<Command>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
    pub condition: CompoundList,
    pub true_part: Option<CompoundList>,
    pub false_part: Option<CompoundList>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UntilLoop {
    pub body: CompoundList,
    pub condition: CompoundList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileLoop {
    pub condition: CompoundList,
    pub body: CompoundList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForEach {
    pub wordlist: Vec<Token>,
    pub body: CompoundList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Redirection {
    File(FileRedirection),
    Fd(FdDuplication),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileRedirection {
    pub fd_number: usize,
    pub file_name: Token,
    /// `<` or `<>`
    pub input: bool,
    /// `>` or `<>`
    pub output: bool,
    /// `>|`
    pub clobber: bool,
    /// `>>`
    pub append: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FdDuplication {
    /// Dup `src_fd_number` ...
    pub src_fd_number: usize,
    /// ... into `dest_fd_number` for the child
    pub dest_fd_number: usize,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SimpleCommand {
    /// Any assignment words to override the environment
    pub assignments: Vec<Token>,
    pub redirections: Vec<Redirection>,
    /// The words that will be expanded to form the argv
    pub words: Vec<Token>,
}

fn display_token_text(t: &Token, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    match &t.kind {
        TokenKind::Word(word) => write!(fmt, "{}", word),
        TokenKind::Name(word) => write!(fmt, "{}", word),
        TokenKind::IoNumber(word) => write!(fmt, "{}", word),
        TokenKind::Eof => Ok(()),
        TokenKind::NewLine => write!(fmt, "\n"),
        TokenKind::Operator(op) => write!(fmt, "{}", op),
        TokenKind::ReservedWord(word) => write!(fmt, "{}", word),
    }
}

impl std::fmt::Display for Command {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match &self.command {
            CommandType::SimpleCommand(cmd) => {
                for t in &cmd.assignments {
                    display_token_text(t, fmt)?;
                    write!(fmt, " ")?;
                }
                for t in &cmd.words {
                    display_token_text(t, fmt)?;
                    write!(fmt, " ")?;
                }
                // TODO: redirections
            }
            CommandType::If(If {
                condition,
                true_part,
                false_part,
            }) => {
                write!(fmt, "if ")?;
                for cmd in &condition.commands {
                    cmd.fmt(fmt)?;
                    write!(fmt, "{}", if cmd.asynchronous { "&" } else { ";" })?;
                }
                write!(fmt, " ; then ")?;
                if let Some(true_part) = true_part {
                    for cmd in &true_part.commands {
                        cmd.fmt(fmt)?;
                        write!(fmt, "{}", if cmd.asynchronous { "&" } else { ";" })?;
                    }
                } else {
                    write!(fmt, ": ")?;
                }
                write!(fmt, " ; else ")?;
                if let Some(false_part) = false_part {
                    for cmd in &false_part.commands {
                        cmd.fmt(fmt)?;
                        write!(fmt, "{}", if cmd.asynchronous { "&" } else { ";" })?;
                    }
                } else {
                    write!(fmt, ": ")?;
                }
                write!(fmt, " ; fi")?;
            }
            CommandType::ForEach(_) | CommandType::UntilLoop(_) | CommandType::WhileLoop(_) => {
                write!(fmt, "Display not implemented; {:?}", self.command)?;
            }
            CommandType::Subshell(commands) => {
                write!(fmt, "(")?;
                for cmd in commands.commands.iter() {
                    cmd.fmt(fmt)?;
                }
                write!(fmt, ")")?;
            }
            CommandType::BraceGroup(commands) => {
                write!(fmt, "{{ ")?;
                for cmd in commands.commands.iter() {
                    cmd.fmt(fmt)?;
                }
                write!(fmt, " }}")?;
            }
            CommandType::Pipeline(pipeline) => {
                for (idx, cmd) in pipeline.commands.iter().enumerate() {
                    if idx > 0 {
                        write!(fmt, "|")?;
                    }
                    cmd.fmt(fmt)?;
                }
            }
        }
        Ok(())
    }
}

impl From<CommandType> for Command {
    fn from(command: CommandType) -> Command {
        Command {
            command,
            redirects: None,
            asynchronous: false,
        }
    }
}

impl From<Pipeline> for Command {
    fn from(pipeline: Pipeline) -> Command {
        // Simplify a pipeline to the command itself if possible
        if !pipeline.inverted && pipeline.commands.len() == 1 {
            pipeline.commands.into_iter().next().unwrap()
        } else {
            CommandType::Pipeline(pipeline).into()
        }
    }
}

impl IntoIterator for CompoundList {
    type Item = Command;
    type IntoIter = ::std::vec::IntoIter<Command>;
    fn into_iter(self) -> Self::IntoIter {
        self.commands.into_iter()
    }
}

impl From<Command> for CompoundList {
    fn from(cmd: Command) -> CompoundList {
        CompoundList {
            commands: vec![cmd],
        }
    }
}

impl SimpleCommand {
    pub fn expand_argv(
        &self,
        env: &mut Environment,
        expander: &Expander,
        aliases: &Aliases,
    ) -> Fallible<Vec<ShellString>> {
        // FIXME: scoped assignments need to return a new env
        let mut argv = vec![];
        for word in &self.words {
            let word = if argv.is_empty() {
                let mut word = word.clone();
                word.apply_command_word_rules(Some(aliases));
                word
            } else {
                word.clone()
            };

            match word.kind {
                TokenKind::Word(ref s) | TokenKind::Name(ref s) => {
                    let mut fields = expander.expand_word(&s.as_str().into(), env)?;
                    argv.append(&mut fields);
                }
                _ => bail!("unhandled token kind {:?}", word),
            }
        }
        Ok(argv)
    }
}
