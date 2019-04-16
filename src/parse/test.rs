
use super::*;
use pretty_assertions::assert_eq;
use shlex::TokenPosition;

fn parse(text: &str) -> Fallible<Command> {
    let mut parser = Parser::new("test", text.as_bytes());
    parser.parse()
}

#[test]
fn test_parse() {
    let list = parse("ls -l foo").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![],
            redirections: vec![],
            words: vec![
                Token::new(
                    TokenKind::new_word("ls"),
                    TokenPosition { line: 0, col: 0 },
                    TokenPosition { line: 0, col: 1 },
                ),
                Token::new(
                    TokenKind::new_word("-l"),
                    TokenPosition { line: 0, col: 3 },
                    TokenPosition { line: 0, col: 4 },
                ),
                Token::new(
                    TokenKind::new_word("foo"),
                    TokenPosition { line: 0, col: 6 },
                    TokenPosition { line: 0, col: 8 },
                )
            ]
        }))
    );
}

#[test]
fn test_parse_two_lines() {
    let list = parse("false\ntrue").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::BraceGroup(CompoundList {
            commands: vec![
                Command::from(CommandType::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    redirections: vec![],
                    words: vec![Token::new(
                        TokenKind::new_word("false"),
                        TokenPosition { line: 0, col: 0 },
                        TokenPosition { line: 0, col: 4 },
                    ),]
                }),),
                Command::from(CommandType::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    redirections: vec![],
                    words: vec![Token::new(
                        TokenKind::new_word("true"),
                        TokenPosition { line: 1, col: 0 },
                        TokenPosition { line: 1, col: 3 },
                    ),]
                }))
            ]
        }))
    );
}

#[test]
fn test_parse_with_alias() {
    let command = parse("ls foo").unwrap();
    let mut aliases = Aliases::new();
    aliases.alias("ls", "ls -l");
    let mut env = Environment::new();
    struct MockExpander {}
    impl Expander for MockExpander {
        fn lookup_homedir(
            &self,
            _user: Option<&str>,
            _env: &mut Environment,
        ) -> Fallible<ShellString> {
            bail!("nope");
        }
    }
    if let Command {
        command: CommandType::SimpleCommand(cmd),
        ..
    } = command
    {
        let argv = cmd
            .expand_argv(&mut env, &MockExpander {}, &aliases)
            .unwrap();
        assert_eq!(
            argv,
            vec![
                "ls".to_string().into(),
                "-l".to_string().into(),
                "foo".to_string().into()
            ]
        );
    } else {
        panic!("wrong command type!?");
    }
}

#[test]
fn redirect_out() {
    let list = parse("echo >foo").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            command: CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![Redirection::File(FileRedirection {
                    fd_number: 1,
                    file_name: Token {
                        kind: TokenKind::new_word("foo"),
                        start: TokenPosition { line: 0, col: 6 },
                        end: TokenPosition { line: 0, col: 8 }
                    },
                    input: false,
                    output: true,
                    clobber: false,
                    append: false
                })],
                words: vec![Token {
                    kind: TokenKind::new_word("echo"),
                    start: TokenPosition { line: 0, col: 0 },
                    end: TokenPosition { line: 0, col: 3 }
                }]
            }),
            redirects: None
        }
    );
}

#[test]
fn redirect_append() {
    let list = parse("echo >>foo").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            command: CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![Redirection::File(FileRedirection {
                    fd_number: 1,
                    file_name: Token {
                        kind: TokenKind::new_word("foo"),
                        start: TokenPosition { line: 0, col: 7 },
                        end: TokenPosition { line: 0, col: 9 }
                    },
                    input: false,
                    output: true,
                    clobber: false,
                    append: true
                })],
                words: vec![Token {
                    kind: TokenKind::new_word("echo"),
                    start: TokenPosition { line: 0, col: 0 },
                    end: TokenPosition { line: 0, col: 3 }
                }]
            }),
            redirects: None
        }
    );
}

#[test]
fn redirect_clobber() {
    let list = parse("echo >|foo").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            command: CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![Redirection::File(FileRedirection {
                    fd_number: 1,
                    file_name: Token {
                        kind: TokenKind::new_word("foo"),
                        start: TokenPosition { line: 0, col: 7 },
                        end: TokenPosition { line: 0, col: 9 }
                    },
                    input: false,
                    output: true,
                    clobber: true,
                    append: false
                })],
                words: vec![Token {
                    kind: TokenKind::new_word("echo"),
                    start: TokenPosition { line: 0, col: 0 },
                    end: TokenPosition { line: 0, col: 3 }
                }]
            }),
            redirects: None
        }
    );
}

#[test]
fn redirect_input() {
    let list = parse("echo <foo").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            command: CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![Redirection::File(FileRedirection {
                    fd_number: 0,
                    file_name: Token {
                        kind: TokenKind::new_word("foo"),
                        start: TokenPosition { line: 0, col: 6 },
                        end: TokenPosition { line: 0, col: 8 }
                    },
                    input: true,
                    output: false,
                    clobber: false,
                    append: false
                })],
                words: vec![Token {
                    kind: TokenKind::new_word("echo"),
                    start: TokenPosition { line: 0, col: 0 },
                    end: TokenPosition { line: 0, col: 3 }
                }]
            }),
            redirects: None
        }
    );
}

#[test]
fn redirect_stderr_stdout() {
    let list = parse("echo 2>&1").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            command: CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![Redirection::Fd(FdDuplication {
                    src_fd_number: 1,
                    dest_fd_number: 2
                })],
                words: vec![Token {
                    kind: TokenKind::new_word("echo"),
                    start: TokenPosition { line: 0, col: 0 },
                    end: TokenPosition { line: 0, col: 3 }
                }]
            }),
            redirects: None
        }
    );
}

#[test]
fn redirect_dup_for_input() {
    let list = parse("echo 0<&1").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            command: CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![Redirection::Fd(FdDuplication {
                    src_fd_number: 1,
                    dest_fd_number: 0
                })],
                words: vec![Token {
                    kind: TokenKind::new_word("echo"),
                    start: TokenPosition { line: 0, col: 0 },
                    end: TokenPosition { line: 0, col: 3 }
                }]
            }),
            redirects: None
        }
    );
}

#[test]
fn redirect_dup_for_input_and_output() {
    let list = parse("echo <>file").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            command: CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![Redirection::File(FileRedirection {
                    fd_number: 0,
                    file_name: Token {
                        kind: TokenKind::new_word("file"),
                        start: TokenPosition { line: 0, col: 7 },
                        end: TokenPosition { line: 0, col: 10 }
                    },
                    input: true,
                    output: true,
                    clobber: false,
                    append: false
                })],
                words: vec![Token {
                    kind: TokenKind::new_word("echo"),
                    start: TokenPosition { line: 0, col: 0 },
                    end: TokenPosition { line: 0, col: 3 }
                }]
            }),
            redirects: None
        }
    );
}

#[test]
fn redirect_fd_not_number() {
    assert_eq!(
        parse("echo <&file")
            .unwrap_err()
            .downcast::<ParseErrorKind>()
            .unwrap(),
        ParseErrorKind::UnexpectedToken(
            Token {
                kind: TokenKind::new_word("file"),
                start: TokenPosition { line: 0, col: 7 },
                end: TokenPosition { line: 0, col: 10 }
            },
            ParseErrorContext::FdRedirectionExpectsNumber
        )
    );
}

#[test]
fn bad_form_subshell() {
    assert_eq!(
        parse("(echo")
            .unwrap_err()
            .downcast::<ParseErrorKind>()
            .unwrap(),
        ParseErrorKind::UnexpectedToken(
            Token {
                kind: TokenKind::Eof,
                start: TokenPosition { line: 0, col: 5 },
                end: TokenPosition { line: 0, col: 5 }
            },
            ParseErrorContext::ExpectingRightParen
        )
    );
}

#[test]
fn subshell_no_space() {
    let list = parse("(echo)").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            redirects: None,
            command: CommandType::Subshell(CompoundList {
                commands: vec![Command {
                    asynchronous: false,
                    command: CommandType::SimpleCommand(SimpleCommand {
                        assignments: vec![],
                        redirections: vec![],
                        words: vec![Token {
                            kind: TokenKind::new_word("echo"),
                            start: TokenPosition { line: 0, col: 1 },
                            end: TokenPosition { line: 0, col: 4 }
                        }]
                    }),
                    redirects: None
                }]
            })
        }
    );
}

#[test]
fn subshell_redirected() {
    let list = parse("(echo)>foo").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            redirects: Some(RedirectList {
                redirections: vec![Redirection::File(FileRedirection {
                    fd_number: 1,
                    file_name: Token {
                        kind: TokenKind::new_word("foo"),
                        start: TokenPosition { line: 0, col: 7 },
                        end: TokenPosition { line: 0, col: 9 }
                    },
                    input: false,
                    output: true,
                    clobber: false,
                    append: false
                })]
            }),

            command: CommandType::Subshell(CompoundList {
                commands: vec![Command {
                    asynchronous: false,
                    command: CommandType::SimpleCommand(SimpleCommand {
                        assignments: vec![],
                        redirections: vec![],
                        words: vec![Token {
                            kind: TokenKind::new_word("echo"),
                            start: TokenPosition { line: 0, col: 1 },
                            end: TokenPosition { line: 0, col: 4 }
                        }]
                    }),
                    redirects: None,
                }]
            })
        }
    );
}

#[test]
fn brace_group_no_spaces() {
    // zsh sees this `{ echo }`, but that is not conformant with the
    // posix shell language spec, which states that `{` is only special
    // when in the command word position.  As a result, the `{` doesn't
    // get reported as a separate token, resulting in an attempt to
    // invoke `{echo}`, which is not typically in the path.
    let list = parse("{echo}").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            command: CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![],
                words: vec![Token {
                    kind: TokenKind::new_word("{echo}"),
                    start: TokenPosition { line: 0, col: 0 },
                    end: TokenPosition { line: 0, col: 5 }
                }]
            }),
            redirects: None
        }
    );
}

#[test]
fn bad_form_brace_group() {
    assert_eq!(
        parse("{ echo")
            .unwrap_err()
            .downcast::<ParseErrorKind>()
            .unwrap(),
        ParseErrorKind::UnexpectedToken(
            Token {
                kind: TokenKind::Eof,
                start: TokenPosition { line: 0, col: 6 },
                end: TokenPosition { line: 0, col: 6 }
            },
            ParseErrorContext::ExpectingRightBrace
        )
    );
}

#[test]
fn brace_group() {
    let list = parse("{ echo }").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            redirects: None,
            command: CommandType::BraceGroup(CompoundList {
                commands: vec![Command {
                    asynchronous: false,
                    command: CommandType::SimpleCommand(SimpleCommand {
                        assignments: vec![],
                        redirections: vec![],
                        words: vec![Token {
                            kind: TokenKind::new_word("echo"),
                            start: TokenPosition { line: 0, col: 2 },
                            end: TokenPosition { line: 0, col: 5 }
                        }]
                    }),
                    redirects: None
                }]
            })
        }
    );
}

#[test]
fn brace_group_sep_list() {
    let list = parse("{ echo ; boo }").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            redirects: None,
            command: CommandType::BraceGroup(CompoundList {
                commands: vec![
                    Command {
                        asynchronous: false,
                        command: CommandType::SimpleCommand(SimpleCommand {
                            assignments: vec![],
                            redirections: vec![],
                            words: vec![Token {
                                kind: TokenKind::new_word("echo"),
                                start: TokenPosition { line: 0, col: 2 },
                                end: TokenPosition { line: 0, col: 5 }
                            }]
                        }),
                        redirects: None
                    },
                    Command {
                        asynchronous: false,
                        command: CommandType::SimpleCommand(SimpleCommand {
                            assignments: vec![],
                            redirections: vec![],
                            words: vec![Token {
                                kind: TokenKind::new_word("boo"),
                                start: TokenPosition { line: 0, col: 9 },
                                end: TokenPosition { line: 0, col: 11 }
                            }]
                        }),
                        redirects: None
                    },
                ]
            })
        }
    );
}

#[test]
fn brace_group_sep_newline() {
    let list = parse("{\n\techo\n\tboo\n}").unwrap();
    assert_eq!(
        list,
        Command {
            asynchronous: false,
            redirects: None,
            command: CommandType::BraceGroup(CompoundList {
                commands: vec![
                    Command {
                        asynchronous: false,
                        command: CommandType::SimpleCommand(SimpleCommand {
                            assignments: vec![],
                            redirections: vec![],
                            words: vec![Token {
                                kind: TokenKind::new_word("echo"),
                                start: TokenPosition { line: 1, col: 1 },
                                end: TokenPosition { line: 1, col: 4 }
                            }]
                        }),
                        redirects: None
                    },
                    Command {
                        asynchronous: false,
                        command: CommandType::SimpleCommand(SimpleCommand {
                            assignments: vec![],
                            redirections: vec![],
                            words: vec![Token {
                                kind: TokenKind::new_word("boo"),
                                start: TokenPosition { line: 2, col: 1 },
                                end: TokenPosition { line: 2, col: 3 }
                            }]
                        }),
                        redirects: None
                    },
                ]
            })
        }
    );
}
