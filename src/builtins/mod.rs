use failure::{err_msg, Fallible};
use lazy_static::lazy_static;
use shell_vm::{Environment, IoEnvironment, Value, WaitableStatus};
use std::collections::HashMap;
use std::path::PathBuf;
use structopt::*;

mod builtins;
mod colon;
mod jobcontrol;
mod truefalse;
mod which;
mod workingdir;

pub trait Builtin: StructOpt {
    fn eval(
        argv: &[Value],
        environment: &mut Environment,
        current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
    ) -> Fallible<WaitableStatus>
    where
        Self: Sized,
    {
        let app = Self::clap()
            .global_setting(structopt::clap::AppSettings::ColoredHelp)
            .global_setting(structopt::clap::AppSettings::DisableVersion)
            .name(Self::name());
        let mut os_args = vec![];
        for arg in argv {
            os_args.push(
                arg.as_os_str()
                    .ok_or_else(|| err_msg("argument is not representable as osstr"))?,
            );
        }
        let mut args = Self::from_clap(&app.get_matches_from_safe(os_args.iter())?);
        args.run(environment, current_directory, io_env)
    }

    fn name() -> &'static str;

    fn run(
        &mut self,
        environment: &mut Environment,
        current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
    ) -> Fallible<WaitableStatus>;
}

pub type BuiltinFunc = fn(
    argv: &[Value],
    environment: &mut Environment,
    current_directory: &mut PathBuf,
    io_env: &IoEnvironment,
) -> Fallible<WaitableStatus>;

pub fn lookup_builtin(name: &Value) -> Option<BuiltinFunc> {
    if let Some(s) = name.as_str() {
        BUILTINS.get(s).map(|f| *f)
    } else {
        None
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
            builtins::BuiltinsCommand,
            which::WhichCommand,
        );

        builtins
    };
}
