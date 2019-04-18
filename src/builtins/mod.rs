use crate::execenv::ExecutionEnvironment;
use crate::exitstatus::ExitStatus;
use failure::Fallible;
use lazy_static::lazy_static;
use shlex::string::ShellString;
use std::collections::HashMap;
use structopt::*;

mod colon;
mod jobcontrol;
mod truefalse;
mod workingdir;

pub trait Builtin: StructOpt {
    fn eval(argv: Vec<ShellString>, exe: &ExecutionEnvironment) -> Fallible<ExitStatus>
    where
        Self: Sized,
    {
        let app = Self::clap()
            .global_setting(structopt::clap::AppSettings::ColoredHelp)
            .global_setting(structopt::clap::AppSettings::DisableVersion)
            .name(Self::name());
        let mut args = Self::from_clap(&app.get_matches_from_safe(argv.iter())?);
        args.run(exe)
    }

    fn name() -> &'static str;

    fn run(&mut self, exe: &ExecutionEnvironment) -> Fallible<ExitStatus>;
}

pub type BuiltinFunc =
    fn(argv: Vec<ShellString>, exe: &ExecutionEnvironment) -> Fallible<ExitStatus>;

pub fn lookup_builtin(name: &ShellString) -> Option<BuiltinFunc> {
    match &name {
        ShellString::String(s) => BUILTINS.get(s.as_str()).map(|f| *f),
        _ => None,
    }
}

lazy_static! {
    static ref BUILTINS: HashMap<&'static str, BuiltinFunc> = {
        let mut builtins = HashMap::new();
        // This identity helper effectively casts away the per-function
        // type information that would otherwise cause a type mismatch
        // when populating the hashmap
        fn identity(f: BuiltinFunc) -> BuiltinFunc {
            f
        }
        macro_rules! builtins {
            ($($CmdType:ty),* $(,)? ) => {
                $(
                builtins.insert(<$CmdType>::name(), identity(<$CmdType>::eval));
                )*
            }
        }

        builtins!(
            jobcontrol::JobsCommand,
            jobcontrol::FgCommand,
            workingdir::CdCommand,
            workingdir::PwdCommand,
            colon::ColonCommand,
            truefalse::TrueCommand,
            truefalse::FalseCommand,
        );

        builtins
    };
}
