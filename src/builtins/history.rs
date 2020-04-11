use crate::builtins::Builtin;
use crate::shellhost::FunctionRegistry;
use anyhow::Context;
use cancel::Token;
use shell_vm::{Environment, IoEnvironment, Status, WaitableStatus};
use sqlite::Value;
use std::borrow::Cow;
use std::convert::TryInto;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::*;
use tabout::{tabulate_output, Alignment, Column};
use termwiz::lineedit::*;

pub struct SqliteHistory {
    connection: sqlite::Connection,
}

pub struct HistoryEntry {
    pub idx: HistoryIndex,
    pub cmd: String,
}

impl SqliteHistory {
    pub fn new() -> Self {
        let path = dirs::home_dir()
            .expect("can't find HOME dir")
            .join(".wzsh-history.db");
        let connection = sqlite::open(&path)
            .with_context(|| format!("initializing history file {}", path.display()))
            .unwrap();
        connection
            .execute("CREATE TABLE if not exists history (cmd TEXT, ts INTEGER)")
            .unwrap();
        Self { connection }
    }

    pub fn get_last_n_entries(&self, n: usize) -> Vec<HistoryEntry> {
        let mut cursor = self
            .connection
            .prepare("select rowid, cmd from history order by rowid desc limit ?")
            .unwrap()
            .cursor();
        cursor
            .bind(&[Value::Integer(n.try_into().unwrap())])
            .unwrap();

        let mut res = vec![];
        while let Ok(Some(row)) = cursor.next() {
            res.push(HistoryEntry {
                idx: row[0].as_integer().unwrap().try_into().unwrap(),
                cmd: row[1].as_string().unwrap().to_string(),
            });
        }
        res.reverse();
        res
    }
}

impl History for SqliteHistory {
    fn get(&self, idx: HistoryIndex) -> Option<Cow<str>> {
        let mut cursor = self
            .connection
            .prepare("select cmd from history where rowid=?")
            .unwrap()
            .cursor();
        cursor
            .bind(&[Value::Integer(idx.try_into().unwrap())])
            .unwrap();
        if let Some(row) = cursor.next().unwrap() {
            Some(Cow::Owned(row[0].as_string().unwrap().to_string()))
        } else {
            None
        }
    }

    fn last(&self) -> Option<HistoryIndex> {
        let mut cursor = self
            .connection
            .prepare("select rowid from history order by rowid desc limit 1")
            .unwrap()
            .cursor();
        if let Some(row) = cursor.next().unwrap() {
            Some(row[0].as_integer().unwrap().try_into().unwrap())
        } else {
            None
        }
    }

    fn add(&mut self, line: &str) {
        if let Some(last_idx) = self.last() {
            if let Some(last_line) = self.get(last_idx) {
                if last_line == line {
                    // Ignore duplicates
                    return;
                }
            }
        }

        let mut cursor = self
            .connection
            .prepare("insert into history values (?, strftime('%s','now'))")
            .unwrap()
            .cursor();
        cursor.bind(&[Value::String(line.to_string())]).unwrap();
        cursor.next().ok();
    }

    fn search(
        &self,
        idx: HistoryIndex,
        style: SearchStyle,
        direction: SearchDirection,
        pattern: &str,
    ) -> Option<SearchResult> {
        let query = match (style, direction) {
            (SearchStyle::Substring, SearchDirection::Backwards) => {
                "select rowid, cmd from history where rowid <= ? and cmd like ? order by rowid desc limit 1"
            }
            (SearchStyle::Substring, SearchDirection::Forwards) => {
                "select rowid, cmd from history where rowid >= ? and cmd like ? order by rowid limit 1"
            }
        };

        let mut cursor = self.connection.prepare(query).unwrap().cursor();
        let params = &[
            Value::Integer(idx.try_into().unwrap()),
            Value::String(format!("%{}%", pattern)),
        ];
        // print!("{} {:?}\r\n", query, params);

        cursor.bind(params).unwrap();
        if let Some(Some(row)) = cursor.next().ok() {
            // print!("row: {:?}\r\n\r\n", row);
            let line = Cow::Owned(row[1].as_string().unwrap().to_string());
            let idx = row[0].as_integer().unwrap();
            if let Some(cursor) = style.match_against(pattern, &line) {
                Some(SearchResult {
                    line,
                    idx: idx.try_into().unwrap(),
                    cursor,
                })
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(StructOpt)]
#[structopt(rename_all = "kebab")]
pub enum HistoryCommand {
    Recent {
        #[structopt(default_value = "16")]
        num_entries: usize,
    },
}

impl Builtin for HistoryCommand {
    fn name() -> &'static str {
        "history"
    }

    fn run(
        &mut self,
        _environment: &mut Environment,
        _current_directory: &mut PathBuf,
        io_env: &IoEnvironment,
        _cancel: Arc<Token>,
        _functions: &Arc<FunctionRegistry>,
    ) -> anyhow::Result<WaitableStatus> {
        match self {
            Self::Recent { num_entries } => {
                let history = SqliteHistory::new();
                let entries = history.get_last_n_entries(*num_entries);

                let columns = [
                    Column {
                        name: "IDX".to_string(),
                        alignment: Alignment::Right,
                    },
                    Column {
                        name: "CMD".to_string(),
                        alignment: Alignment::Left,
                    },
                ];

                let rows: Vec<_> = entries
                    .into_iter()
                    .map(|entry| vec![entry.idx.to_string(), entry.cmd])
                    .collect();

                tabulate_output(&columns, &rows, &mut io_env.stdout())?;
            }
        }
        Ok(Status::Complete(0.into()).into())
    }
}
