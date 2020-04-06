use chrono::prelude::*;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
#[cfg(unix)]
use std::sync::Mutex;
use structopt::*;
use tabout::{tabulate_for_terminal, unicode_column_width_of_change_slice, Alignment, Column};
use termwiz::caps::{Capabilities, ProbeHints};
use termwiz::cell::{CellAttributes, Intensity};
use termwiz::color::AnsiColor;
use termwiz::surface::Change;
use termwiz::terminal::{new_terminal, Terminal};

#[cfg(unix)]
lazy_static::lazy_static! {
    pub static ref UID_TO_NAME: Mutex<HashMap<u32, String>> = Mutex::new(HashMap::new());
    pub static ref GID_TO_NAME: Mutex<HashMap<u32, String>> = Mutex::new(HashMap::new());
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl FileType {
    pub fn is_symlink(self) -> bool {
        FileType::Symlink == self
    }

    pub fn render_attributes(self) -> CellAttributes {
        match self {
            FileType::File => CellAttributes::default(),
            FileType::Directory => CellAttributes::default()
                .set_foreground(AnsiColor::Blue)
                .set_intensity(Intensity::Bold)
                .clone(),
            FileType::Symlink => CellAttributes::default()
                .set_foreground(AnsiColor::Aqua)
                .clone(),
        }
    }
}

/// Resolve a sid to a string of the form `DOMAIN\name`
#[cfg(windows)]
fn lookup_account_sid(sid: winapi::um::winnt::PSID) -> anyhow::Result<String> {
    use anyhow::Context as _;
    use std::ffi::OsString;
    use std::os::windows::ffi::OsStringExt;
    unsafe {
        let mut name = vec![0u16; 1024];
        let mut name_len = name.len() as _;
        let mut domain_name = vec![0u16; 1024];
        let mut domain_name_len = domain_name.len() as _;
        let mut sid_use = 0;

        let res = winapi::um::winbase::LookupAccountSidW(
            std::ptr::null_mut(),
            sid,
            name.as_mut_ptr(),
            &mut name_len,
            domain_name.as_mut_ptr(),
            &mut domain_name_len,
            &mut sid_use,
        );

        if res == 0 {
            return Err(std::io::Error::last_os_error()).context("LookupAccountSidW");
        }

        name.resize(name_len as usize, 0u16);
        domain_name.resize(domain_name_len as usize, 0u16);

        let name = OsString::from_wide(&name);
        let domain_name = OsString::from_wide(&domain_name);
        Ok(format!(
            "{}\\{}",
            domain_name.to_string_lossy(),
            name.to_string_lossy()
        ))
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

    #[cfg(not(unix))]
    pub fn owner_and_group(&self) -> anyhow::Result<(String, String)> {
        use anyhow::Context as _;
        use std::fs::OpenOptions;
        use std::os::windows::prelude::*;

        let file = OpenOptions::new()
            .access_mode(winapi::um::winnt::READ_CONTROL)
            .custom_flags(
                winapi::um::winbase::FILE_FLAG_POSIX_SEMANTICS
                    | winapi::um::winbase::FILE_FLAG_BACKUP_SEMANTICS,
            )
            .open(&self.name)?;

        unsafe {
            let mut sid_owner = std::ptr::null_mut();
            let mut sid_group = std::ptr::null_mut();
            let mut sd = std::ptr::null_mut();
            let res = winapi::um::aclapi::GetSecurityInfo(
                file.as_raw_handle(),
                winapi::um::accctrl::SE_FILE_OBJECT,
                winapi::um::winnt::OWNER_SECURITY_INFORMATION
                    | winapi::um::winnt::GROUP_SECURITY_INFORMATION,
                &mut sid_owner,
                &mut sid_group,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                &mut sd,
            );

            if res != winapi::shared::winerror::ERROR_SUCCESS {
                return Err(std::io::Error::last_os_error()).context("GetSecurityInfo");
            }

            // Some guff to ensure that we free the descriptor
            struct DropLocalFree(*mut std::ffi::c_void);
            impl Drop for DropLocalFree {
                fn drop(&mut self) {
                    unsafe {
                        winapi::um::winbase::LocalFree(self.0);
                    }
                }
            }
            let _security_descriptor = DropLocalFree(sd);

            anyhow::ensure!(
                !sid_owner.is_null(),
                "GetSecurityInfo succeeded but didn't return sid_owner"
            );
            anyhow::ensure!(
                !sid_group.is_null(),
                "GetSecurityInfo succeeded but didn't return sid_group"
            );

            let owner = lookup_account_sid(sid_owner)?;
            let group = lookup_account_sid(sid_group)?;

            Ok((owner, group))
        }
    }

    #[cfg(unix)]
    pub fn owner_and_group(&self) -> anyhow::Result<(String, String)> {
        Ok((self.owner(), self.group()))
    }

    #[cfg(unix)]
    fn owner(&self) -> String {
        use std::os::unix::fs::MetadataExt;
        let meta = self.meta.as_ref().unwrap();

        if let Some(name) = UID_TO_NAME.lock().unwrap().get(&meta.uid()) {
            return name.clone();
        }

        let name = unsafe {
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
        };

        UID_TO_NAME.lock().unwrap().insert(meta.uid(), name.clone());
        name
    }

    #[cfg(unix)]
    fn group(&self) -> String {
        use std::os::unix::fs::MetadataExt;
        let meta = self.meta.as_ref().unwrap();

        if let Some(name) = GID_TO_NAME.lock().unwrap().get(&meta.gid()) {
            return name.clone();
        }

        let name = unsafe {
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
        };

        GID_TO_NAME.lock().unwrap().insert(meta.gid(), name.clone());
        name
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
        "r-xr-xr-x."
    } else {
        "rwxrwxrwx."
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
fn nlink_from_metadata(_meta: &std::fs::Metadata) -> String {
    /* TODO: this is nightly only, we'll need to go direct.
    use std::os::windows::fs::MetadataExt;
    meta.number_of_links().unwrap_or(1).to_string()
    */
    "1".to_string()
}

#[cfg(unix)]
fn nlink_from_metadata(meta: &std::fs::Metadata) -> String {
    use std::os::unix::fs::MetadataExt;
    meta.nlink().to_string()
}

impl LsCommand {
    fn consider(
        &self,
        name: &Path,
        top_level: bool,
        results: &mut HashMap<PathBuf, Vec<Data>>,
        opt_file_type: Option<FileType>,
    ) -> anyhow::Result<()> {
        if let Some(base_name) = name.file_name().map(|n| n.to_string_lossy()) {
            if base_name.starts_with(".") && !self.all && !top_level {
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
                        if let Ok(entry) = entry {
                            self.consider(
                                &entry.path(),
                                false,
                                results,
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
        current_directory: &Path,
        terminal: &mut Option<impl Terminal>,
    ) -> anyhow::Result<()> {
        let mut output = vec![];

        let display_dir = self.relative_to_dir(dir, current_directory, current_directory);
        if print_dir_name {
            output.push(format!("{}:\r\n", display_dir.display()).into());
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
            let name = self.relative_to_dir(&entry.name, dir, current_directory);
            let classification = if self.classify { entry.classify() } else { "" };

            let display = vec![
                Change::AllAttributes(entry.file_type.render_attributes()),
                name.display().to_string().into(),
                Change::AllAttributes(CellAttributes::default()),
                classification.into(),
            ];
            let width = unicode_column_width_of_change_slice(&display);

            values.push((display, width));
        }

        if !self.single_column && !self.long_format && terminal.is_some() {
            // Figure out how many columns we need to fit the available width
            let terminal = terminal.as_mut().unwrap();
            let max_width = terminal.get_screen_size()?.cols;

            let spacing = 2;

            for i in 1..values.len() + 1 {
                let num_cols = (values.len() as f32 / i as f32).ceil() as usize;
                let columns: Vec<&[(Vec<Change>, usize)]> =
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
                                output.extend_from_slice(&display);
                                output.push(pad.into());
                            }
                        }
                        output.push("\r\n".into());
                    }
                    break;
                }
            }

            // We shouldn't get here, but we can if one or more file names
            // are wider than the available terminal width.
            // Fall back to single column output.
            for (display, _width) in values {
                output.extend_from_slice(&display);
                output.push("\r\n".into());
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

            let mut rows: Vec<Vec<Vec<Change>>> = vec![];

            for ((display, _width), entry) in values.iter().zip(entries.iter()) {
                if entry.meta.is_some() {
                    let meta = entry.meta.as_ref().unwrap();

                    let file_size = meta.len();
                    let file_size = if self.human_readable {
                        byte_format(file_size)
                    } else {
                        file_size.to_string()
                    };

                    let perms = permission_format(entry.file_type, meta.permissions());

                    let (owner, group) = entry.owner_and_group()?;

                    let modified: DateTime<Local> = entry.modified.unwrap().into();
                    let modified = modified.format("%b %e %Y %H:%M").to_string();

                    let nlink = nlink_from_metadata(&meta);

                    let mut display = display.clone();

                    if entry.file_type.is_symlink() {
                        display.push(" -> ".into());
                        display.push(
                            entry
                                .name
                                .read_link()
                                .map(|p| p.to_string_lossy().to_owned().to_string())
                                .unwrap_or("".to_string())
                                .into(),
                        );
                    }

                    rows.push(vec![
                        vec![perms.into()],
                        vec![nlink.into()],
                        vec![owner.into()],
                        vec![group.into()],
                        vec![file_size.into()],
                        vec![modified.into()],
                        display,
                    ]);
                } else {
                    output.extend_from_slice(&display);
                    output.push("\r\n".into());
                }
            }

            tabulate_for_terminal(&columns, &rows, CellAttributes::default(), &mut output);
        } else {
            for (display, _width) in values {
                output.extend_from_slice(&display);
                output.push("\r\n".into());
            }
        }

        if print_dir_name {
            output.push("\r\n".into());
        }

        match terminal {
            Some(t) => t.render(&output)?,
            None => {
                for c in output {
                    if c.is_text() {
                        print!("{}", c.text());
                    }
                }
            }
        }

        Ok(())
    }

    fn run(&self) -> anyhow::Result<()> {
        let hints = ProbeHints::new_from_env().mouse_reporting(Some(false));
        let caps = Capabilities::new_with_hints(hints)?;
        let mut terminal = new_terminal(caps).ok();
        let current_directory = std::env::current_dir()?;

        let mut results = HashMap::new();

        if self.files.is_empty() {
            self.consider(&current_directory, true, &mut results, None)?;
        } else {
            for file in &self.files {
                self.consider(file, true, &mut results, None)?;
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
                &current_directory.clone(),
                &mut terminal,
            )?;
        }

        Ok(())
    }
}

fn main() -> anyhow::Result<()> {
    let ls = LsCommand::from_args();
    ls.run()
}
