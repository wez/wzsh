/// https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_10_02
use shell_lexer::{Assignment, Token};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SimpleCommand {
    pub assignments: Vec<Assignment>,
    pub words: Vec<Token>,
    pub redirections: Vec<Redirection>,
}

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
    Program(CompoundList),
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

impl From<Command> for CompoundList {
    fn from(cmd: Command) -> CompoundList {
        CompoundList {
            commands: vec![cmd],
        }
    }
}
