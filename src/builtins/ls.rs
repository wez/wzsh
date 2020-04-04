use crate::builtins::Builtin;
use crate::shellhost::FunctionRegistry;
use cancel::Token;
use chrono::prelude::*;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use structopt::*;
use tabout::{tabulate_output, Alignment, Column};
use termwiz::cell::unicode_column_width;
use termwiz::terminal::{SystemTerminal, Terminal};

#[derive(StructOpt)]
/// List directory contents
pub struct LsCommand {
    /// List information about these files, or if omitted, the
    /// current directory.
    files: Vec<PathBuf>,

    /// do not ignore entries starting with .
    #[structopt(short = "a", long = "all")]
    all: bool,

    /// list directories themselves, not their contents
    #[structopt(short = "d", long = "directory")]
    directory: bool,

    /// append indicator (one of */=>@|) to entries
    #[structopt(short = "F", long = "classify")]
    classify: bool,

    /// use a long listing format
    #[structopt(short = "l")]
    long_format: bool,

    /// Output using a single column
    #[structopt(short = "1")]
    single_column: bool,

    /// When using long listing, print sizes like 1K 234M 2G etc.
    #[structopt(short = "h", long = "human-readable")]
    human_readable: bool,

    /// when showing file information for a symbolic link, show
    /// information for the file the link references rather than
    /// for the link itself
    #[structopt(short = "L", long = "dereference")]
    dereference: bool,

    /// reverse order while sorting
    #[structopt(short = "r", long = "reverse")]
    reverse: bool,

    /// list subdirectories recursively
    #[structopt(short = "R", long = "recursive")]
    recursive: bool,

    /// sort by modification time, newest first
    #[structopt(short = "t")]
    sort_by_time: bool,
}

#[derive(Debug, Clone, Copy)]
enum FileType {
    File,
    Directory,
    Symlink,
}

impl From<std::fs::FileType> for FileType {
    fn from(ft: std::fs::FileType) -> FileType {
        if ft.is_symlink() {
            FileType::Symlink
        } else if ft.is_dir() {
            FileType::Directory
        } else if ft.is_file() {
            FileType::File
        } else {
            unreachable!();
        }
    }
}

#[derive(Debug)]
struct Data {
    name: PathBuf,
    file_type: FileType,
    meta: Option<std::fs::Metadata>,
    modified: Option<std::time::SystemTime>,
    error: Option<String>,
}

impl Data {
    pub fn classify(&self) -> &'static str {
        match &self.file_type {
            FileType::File => "",
            FileType::Symlink => "@",
            FileType::Directory => "/",
        }
    }
}

/// Given a size in bytes, return a human friendly string that makes
/// it easier to understand the magnitude
fn byte_format(size: u64) -> String {
    const KB: u64 = 1_024;
    static UNITS: &'static str = "KMGTPE";

    if size < KB {
        size.to_string()
    } else {
        let size = size as f64;
        let exp = match (size.ln() / (KB as f64).ln()) as usize {
            e if e == 0 => 1,
            e => e,
        };

        format!(
            "{:.1}{}",
            (size / KB.pow(exp as u32) as f64),
            UNITS.as_bytes()[exp - 1] as char,
        )
    }
}

#[cfg(not(unix))]
fn permission_mode(perms: std::fs::Permissions) -> String {
    let mode = if perms.readonly() {
        "r--r--r--."
    } else {
        "rw-rw-rw-."
    };

    mode.to_string()
}

#[cfg(unix)]
fn permission_mode(perms: std::fs::Permissions) -> String {
    use std::os::unix::fs::PermissionsExt;

    let mode = perms.mode() as libc::mode_t;

    let u_r = if mode & libc::S_IRUSR != 0 { 'r' } else { '-' };
    let u_w = if mode & libc::S_IWUSR != 0 { 'w' } else { '-' };
    let u_x = if mode & libc::S_IXUSR != 0 { 'x' } else { '-' };

    let g_r = if mode & libc::S_IRGRP != 0 { 'r' } else { '-' };
    let g_w = if mode & libc::S_IWGRP != 0 { 'w' } else { '-' };
    let g_x = if mode & libc::S_IXGRP != 0 { 'x' } else { '-' };

    let o_r = if mode & libc::S_IROTH != 0 { 'r' } else { '-' };
    let o_w = if mode & libc::S_IWOTH != 0 { 'w' } else { '-' };
    let o_x = if mode & libc::S_IXOTH != 0 { 'x' } else { '-' };

    // FIXME: setuid, setgid and sticky bits

    format!(
        "{}{}{}{}{}{}{}{}{}.",
        u_r, u_w, u_x, g_r, g_w, g_x, o_r, o_w, o_x
    )
}

fn permission_format(file_type: FileType, perms: std::fs::Permissions) -> String {
    let first_char = match file_type {
        FileType::File => '-',
        FileType::Symlink => 'l',
        FileType::Directory => 'd',
    };

    let mode = permission_mode(perms);

    format!("{}{}", first_char, mode)
}

#[cfg(not(unix))]
fn owner_from_metadata(meta: &std::fs::Metadata) -> String {
    "me".to_string()
}

#[cfg(not(unix))]
fn group_from_metadata(meta: &std::fs::Metadata) -> String {
    "".to_string()
}

#[cfg(not(unix))]
fn nlink_from_metadata(meta: &std::fs::Metadata) -> String {
    "1".to_string()
}

#[cfg(unix)]
fn nlink_from_metadata(meta: &std::fs::Metadata) -> String {
    use std::os::unix::fs::MetadataExt;
    meta.nlink().to_string()
}

#[cfg(unix)]
fn owner_from_metadata(meta: &std::fs::Metadata) -> String {
    use std::os::unix::fs::MetadataExt;

    unsafe {
        let mut buf = [0i8; 2048];
        let mut pwbuf: libc::passwd = std::mem::zeroed();
        let mut pw = std::ptr::null_mut();
        libc::getpwuid_r(meta.uid(), &mut pwbuf, buf.as_mut_ptr(), buf.len(), &mut pw);

        if pw.is_null() {
            meta.uid().to_string()
        } else {
            let name = std::ffi::CStr::from_ptr((*pw).pw_name);
            name.to_string_lossy().to_string()
        }
    }
}

#[cfg(unix)]
fn group_from_metadata(meta: &std::fs::Metadata) -> String {
    use std::os::unix::fs::MetadataExt;

    unsafe {
        let mut buf = [0i8; 2048];
        let mut grbuf: libc::group = std::mem::zeroed();
        let mut group = std::ptr::null_mut();
        libc::getgrgid_r(
            meta.gid(),
            &mut grbuf,
            buf.as_mut_ptr(),
            buf.len(),
            &mut group,
        );

        if group.is_null() {
            meta.gid().to_string()
        } else {
            let name = std::ffi::CStr::from_ptr((*group).gr_name);
            name.to_string_lossy().to_string()
        }
    }
}

impl LsCommand {
    fn consider(
        &self,
        name: &Path,
        top_level: bool,
        results: &mut HashMap<PathBuf, Vec<Data>>,
        cancel: &Arc<Token>,
        opt_file_type: Option<FileType>,
    ) -> anyhow::Result<()> {
        cancel.check_cancel()?;

        if let Some(base_name) = name.file_name().map(|n| n.to_string_lossy()) {
            if base_name.starts_with(".") && !self.all {
                return Ok(());
            }
        }

        let mut meta = None;
        let mut error = None;

        let file_type: FileType = match opt_file_type {
            Some(f) => f,
            None => {
                match std::fs::symlink_metadata(name) {
                    Ok(m) => {
                        let ft = m.file_type().into();
                        meta = Some(m);
                        ft
                    }
                    Err(err) => {
                        error = Some(err.to_string());
                        // We don't know the type, but consider it to
                        // be a regular file
                        FileType::File
                    }
                }
            }
        };

        match file_type {
            FileType::Directory if !self.directory && (top_level || self.recursive) => {
                if let Ok(dir) = std::fs::read_dir(name) {
                    for entry in dir {
                        cancel.check_cancel()?;
                        if let Ok(entry) = entry {
                            self.consider(
                                &entry.path(),
                                false,
                                results,
                                cancel,
                                entry.file_type().ok().map(Into::into),
                            )?;
                        }
                    }
                }
            }
            FileType::Symlink if self.dereference => {
                let meta = std::fs::metadata(name)?;
                let file_type: FileType = meta.file_type().into();

                let entry = results
                    .entry(name.parent().unwrap().to_path_buf())
                    .or_insert_with(Vec::new);

                entry.push(Data {
                    name: name.to_path_buf(),
                    file_type,
                    meta: Some(meta.clone()),
                    modified: if self.sort_by_time || self.long_format {
                        meta.modified().ok()
                    } else {
                        None
                    },
                    error,
                });
            }
            _ => {
                let entry = results
                    .entry(name.parent().unwrap().to_path_buf())
                    .or_insert_with(Vec::new);

                let meta = match (self.sort_by_time || self.long_format, meta) {
                    (true, None) => std::fs::symlink_metadata(name).ok(),
                    (_, meta) => meta,
                };
                let modified = meta.as_ref().and_then(|m| m.modified().ok());

                entry.push(Data {
                    name: name.to_path_buf(),
                    file_type,
                    meta: meta.clone(),
                    modified,
                    error,
                });
            }
        }

        Ok(())
    }

    fn relative_to_dir<'a>(
        &self,
        path: &'a Path,
        dir: &Path,
        current_directory: &Path,
    ) -> &'a Path {
        if path == current_directory {
            Path::new(".")
        } else if path.starts_with(&dir) {
            path.strip_prefix(&dir).unwrap()
        } else {
            path
        }
    }

    fn print_dir(
        &self,
        dir: &Path,
        print_dir_name: bool,
        mut entries: Vec<Data>,
        io_env: &IoEnvironment,
        current_directory: &Path,
        cancel: &Arc<Token>,
        terminal: &mut Option<SystemTerminal>,
    ) -> anyhow::Result<()> {
        let display_dir = self.relative_to_dir(dir, current_directory, current_directory);
        if print_dir_name {
            writeln!(io_env.stdout(), "{}:", display_dir.display())?;
        }

        if self.sort_by_time {
            entries.sort_by(|a, b| a.modified.cmp(&b.modified));
        } else {
            entries.sort_by(|a, b| a.name.cmp(&b.name));
        }
        if self.reverse {
            entries.reverse();
        }

        let mut values = vec![];
        for entry in &entries {
            cancel.check_cancel()?;
            let name = self.relative_to_dir(&entry.name, dir, current_directory);
            let classification = if self.classify { entry.classify() } else { "" };

            let display = format!("{}{}", name.display(), classification);
            let width = unicode_column_width(&display);

            values.push((display, width));
        }

        if !self.single_column && !self.long_format && terminal.is_some() {
            // Figure out how many columns we need to fit the available width
            let terminal = terminal.as_mut().unwrap();
            let max_width = terminal.get_screen_size()?.cols;

            let spacing = 2;

            for i in 1..values.len() + 1 {
                let num_cols = (values.len() as f32 / i as f32).ceil() as usize;
                let columns: Vec<&[(String, usize)]> =
                    values.chunks(values.len() / num_cols).collect();
                let widths: Vec<usize> = columns
                    .iter()
                    .map(|rows| {
                        rows.iter()
                            .map(|(_display, width)| width + spacing)
                            .max()
                            .unwrap_or(0)
                    })
                    .collect();

                let total_width: usize = widths.iter().sum();

                if total_width < max_width {
                    for row_number in 0..columns[0].len() {
                        for (column, width) in columns.iter().zip(widths.iter()) {
                            if column.len() > row_number {
                                let (display, item_width) = &column[row_number];
                                let mut pad = String::new();
                                for _ in *item_width..*width {
                                    pad.push(' ');
                                }
                                write!(io_env.stdout(), "{}{}", display, pad)?;
                            }
                        }
                        writeln!(io_env.stdout())?;
                    }
                    break;
                }
            }
        } else if self.long_format {
            let columns = [
                Column {
                    name: "PERMS".to_string(),
                    alignment: Alignment::Left,
                },
                Column {
                    name: "NL".to_string(),
                    alignment: Alignment::Right,
                },
                Column {
                    name: "OWNER".to_string(),
                    alignment: Alignment::Left,
                },
                Column {
                    name: "GROUP".to_string(),
                    alignment: Alignment::Left,
                },
                Column {
                    name: "SIZE".to_string(),
                    alignment: Alignment::Right,
                },
                Column {
                    name: "MODIFIED".to_string(),
                    alignment: Alignment::Left,
                },
                Column {
                    name: "NAME".to_string(),
                    alignment: Alignment::Left,
                },
            ];

            let mut rows = vec![];

            for ((display, _width), entry) in values.iter().zip(entries.iter()) {
                cancel.check_cancel()?;
                if entry.meta.is_some() {
                    let meta = entry.meta.as_ref().unwrap();

                    let file_size = meta.len();
                    let file_size = if self.human_readable {
                        byte_format(file_size)
                    } else {
                        file_size.to_string()
                    };

                    let perms = permission_format(entry.file_type, meta.permissions());

                    let owner = owner_from_metadata(&meta);
                    let group = group_from_metadata(&meta);

                    let modified: DateTime<Local> = entry.modified.unwrap().into();
                    let modified = modified.format("%b %e %Y %H:%M").to_string();

                    let nlink = nlink_from_metadata(&meta);

                    rows.push(vec![
                        perms,
                        nlink,
                        owner,
                        group,
                        file_size,
                        modified,
                        display.to_string(),
                    ]);
                } else {
                    writeln!(io_env.stdout(), "{}", display)?;
                }
            }

            tabulate_output(&columns, &rows, &mut io_env.stdout())?;
        } else {
            for (display, _width) in values {
                cancel.check_cancel()?;
                writeln!(io_env.stdout(), "{}", display)?;
            }
        }

        if print_dir_name {
            writeln!(io_env.stdout())?;
        }

        Ok(())
    }
}

impl Builtin for LsCommand {
    fn name() -> &'static str {
        "ls"
    }

    fn run(
        &mut self,
        _environment: &mut Environment,
        current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> anyhow::Result<WaitableStatus> {
        let mut terminal = super::terminal_from_ioenv(io_env, None).ok();

        let mut results = HashMap::new();

        if self.files.is_empty() {
            self.consider(current_directory, true, &mut results, &cancel, None)?;
        } else {
            for file in &self.files {
                self.consider(file, true, &mut results, &cancel, None)?;
            }
        }

        let print_dir_name = results.len() > 1;
        let mut names: Vec<PathBuf> = results.keys().cloned().collect();
        names.sort();
        if self.reverse {
            names.reverse();
        }

        for name in names {
            let entries = results.remove(&name).unwrap();
            self.print_dir(
                &name,
                print_dir_name,
                entries,
                io_env,
                &current_directory.clone(),
                &cancel,
                &mut terminal,
            )?;
        }

        Ok(Status::Complete(0.into()).into())
    }
}
