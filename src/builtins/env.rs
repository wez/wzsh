use crate::builtins::Builtin;
use crate::shellhost::FunctionRegistry;
use cancel::Token;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::collections::HashSet;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::*;

#[derive(StructOpt)]
/// Set the export attribute for variables (note that this is always
/// set in the current version of wzsh)
pub struct ExportCommand {
    names: Vec<String>,
    /// Print exported variables in a syntax compatible with the shell
    #[structopt(short = "p", conflicts_with = "names")]
    print: bool,
}

impl Builtin for ExportCommand {
    fn name() -> &'static str {
        "export"
    }

    fn run(
        &mut self,
        environment: &mut Environment,
        _current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> anyhow::Result<WaitableStatus> {
        if self.print {
            for (k, v) in environment.iter() {
                cancel.check_cancel()?;
                match (k.to_str(), v.to_str()) {
                    (Some(k), Some(v)) => writeln!(io_env.stdout(), "export {}={}", k, v)?,
                    _ => writeln!(
                        io_env.stderr(),
                        "{:?}={:?} cannot be formatted as utf-8",
                        k,
                        v
                    )?,
                }
            }
        } else {
            for name in &self.names {
                // parse `name=value` and assign
                let split: Vec<&str> = name.splitn(2, '=').collect();
                if split.len() == 2 {
                    environment.set(split[0], split[1]);
                }
            }
        }
        Ok(Status::Complete(0.into()).into())
    }
}

#[derive(StructOpt)]
/// Unset a variable from the environment
pub struct UnsetCommand {
    names: Vec<String>,
}

impl Builtin for UnsetCommand {
    fn name() -> &'static str {
        "unset"
    }

    fn run(
        &mut self,
        environment: &mut Environment,
        _current_directory: &mut PathBuf,
        _io_env: &IoEnvironment,
        _cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> anyhow::Result<WaitableStatus> {
        for name in &self.names {
            environment.unset(name);
        }
        Ok(Status::Complete(0.into()).into())
    }
}

#[derive(StructOpt)]
#[structopt(rename_all = "kebab")]
pub struct PathSpec {
    dirs: Vec<PathBuf>,
    /// If set, remove duplicate path entries
    #[structopt(long)]
    dedup: bool,
    /// If set, don't add an entry that isn't a directory.
    /// This allow for speculatively adding a list of paths
    /// and not cluttering up the path list.
    #[structopt(long)]
    only_existing: bool,
}

enum PathOp {
    Append,
    Prepend,
    Remove,
}

impl PathSpec {
    fn apply(&self, env: &mut Environment, op: PathOp) -> anyhow::Result<()> {
        let mut set = HashSet::new();
        let mut pathvec = Vec::new();

        if let Some(path) = env.get("PATH") {
            for entry in std::env::split_paths(path) {
                if !self.dedup || !set.contains(&entry) {
                    pathvec.push(entry.clone());
                    set.insert(entry);
                }
            }
        }

        for entry in &self.dirs {
            match op {
                PathOp::Append => {
                    if !self.dedup || !set.contains(entry) {
                        let entry = entry.to_path_buf();
                        set.insert(entry.clone());
                        if !self.only_existing || entry.is_dir() {
                            pathvec.push(entry);
                        }
                    }
                }
                PathOp::Prepend => {
                    if !self.dedup || !set.contains(entry) {
                        let entry = entry.to_path_buf();
                        set.insert(entry.clone());
                        if !self.only_existing || entry.is_dir() {
                            pathvec.insert(0, entry);
                        }
                    }
                }
                PathOp::Remove => {
                    pathvec.retain(|p| p != entry);
                }
            }
        }

        let new_path = std::env::join_paths(pathvec.into_iter())?;
        env.set("PATH", new_path);
        Ok(())
    }
}

#[derive(StructOpt)]
#[structopt(rename_all = "kebab")]
/// Ever felt like a caveman when it comes to updating your PATH?
/// This builtin command makes path manipulation a bit more civilized
/// by offering append, prepend, removal and fixup operations to keep
/// your path tidy
pub enum PathCommand {
    /// Print the path elements
    Show,
    /// Add to the end of the path
    Add(PathSpec),
    /// Prepend to the path
    Prepend(PathSpec),
    /// Remove from the path
    Remove(PathSpec),
    /// Remove non-existent entries from the path and de-dup
    Fixup,
}

impl Builtin for PathCommand {
    fn name() -> &'static str {
        "path"
    }

    fn run(
        &mut self,
        env: &mut Environment,
        _current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        _cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> anyhow::Result<WaitableStatus> {
        match self {
            PathCommand::Show => {
                if let Some(path) = env.get("PATH") {
                    for entry in std::env::split_paths(path) {
                        writeln!(io_env.stdout(), "{}", entry.display())?;
                    }
                } else {
                    writeln!(io_env.stderr(), "PATH environment is not set!")?;
                    return Ok(Status::Complete(1.into()).into());
                }
            }
            PathCommand::Add(spec) => spec.apply(env, PathOp::Append)?,
            PathCommand::Prepend(spec) => spec.apply(env, PathOp::Prepend)?,
            PathCommand::Remove(spec) => spec.apply(env, PathOp::Remove)?,
            PathCommand::Fixup => {
                let mut set = HashSet::new();
                let mut pathvec = Vec::new();

                if let Some(path) = env.get("PATH") {
                    for entry in std::env::split_paths(path) {
                        if !set.contains(&entry) {
                            let entry = entry.to_path_buf();
                            set.insert(entry.clone());
                            if entry.is_dir() {
                                pathvec.push(entry);
                            }
                        }
                    }
                }
                let new_path = std::env::join_paths(pathvec.into_iter())?;
                env.set("PATH", new_path);
            }
        }
        Ok(Status::Complete(0.into()).into())
    }
}
