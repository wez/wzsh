use super::*;
use failure::Fallible;
use pretty_assertions::assert_eq;
use shell_lexer::{Assignment, Pos, Span, Token, WordComponent, WordComponentKind};

fn parse(text: &str) -> Fallible<Command> {
    let mut parser = Parser::new(text.as_bytes());
    parser.parse()
}

#[test]
fn test_assign() {
    let list = parse("FOO=bar BAR=baz echo WOOT=woot").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![
                Assignment {
                    name: "FOO".to_owned(),
                    span: Span::new_to(0, 0, 4),
                    value: vec![WordComponent {
                        kind: WordComponentKind::literal("bar"),
                        span: Span::new_to(0, 4, 6),
                        splittable: true,
                        remove_backslash: true
                    }]
                },
                Assignment {
                    name: "BAR".to_owned(),
                    span: Span::new_to(0, 8, 12),
                    value: vec![WordComponent {
                        kind: WordComponentKind::literal("baz"),
                        span: Span::new_to(0, 12, 14),
                        splittable: true,
                        remove_backslash: true
                    }]
                },
            ],
            redirections: vec![],
            words: vec![
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("echo"),
                    span: Span::new_to(0, 16, 19),
                    splittable: true,
                    remove_backslash: true
                }]),
                Token::Word(vec![
                    WordComponent {
                        kind: WordComponentKind::literal("WOOT="),
                        span: Span::new_to(0, 21, 26),
                        splittable: true,
                        remove_backslash: false
                    },
                    WordComponent {
                        kind: WordComponentKind::literal("woot"),
                        span: Span::new_to(0, 26, 29),
                        splittable: true,
                        remove_backslash: true
                    },
                ]),
            ]
        }))
    );
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
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("ls"),
                    span: Span::new_to(0, 0, 1),
                    splittable: true,
                    remove_backslash: true
                }]),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("-l"),
                    span: Span::new_to(0, 3, 4),
                    splittable: true,
                    remove_backslash: true
                }]),
                Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 6, 8),
                    splittable: true,
                    remove_backslash: true
                }]),
            ]
        }))
    );
}

#[test]
fn test_two_lines() {
    let list = parse("false\ntrue").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::Program(CompoundList {
            commands: vec![
                Command::from(CommandType::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    redirections: vec![],
                    words: vec![Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("false"),
                        span: Span::new_to(0, 0, 4),
                        splittable: true,
                        remove_backslash: true
                    }]),]
                })),
                Command::from(CommandType::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    redirections: vec![],
                    words: vec![Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("true"),
                        span: Span::new_to(1, 0, 3),
                        splittable: true,
                        remove_backslash: true
                    }]),]
                }))
            ]
        }))
    );
}

#[test]
fn redirect_out() {
    let list = parse("echo >foo").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![],
            redirections: vec![Redirection::File(FileRedirection {
                fd_number: 1,
                file_name: Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 6, 8),
                    remove_backslash: true,
                    splittable: true,
                }]),
                input: false,
                output: true,
                clobber: false,
                append: false
            })],
            words: vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("echo"),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: true
            }]),]
        }))
    );
}

#[test]
fn redirect_append() {
    let list = parse("echo >>foo").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![],
            redirections: vec![Redirection::File(FileRedirection {
                fd_number: 1,
                file_name: Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 7, 9),
                    remove_backslash: true,
                    splittable: true,
                }]),
                input: false,
                output: true,
                clobber: false,
                append: true
            })],
            words: vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("echo"),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: true
            }]),]
        }))
    );
}

#[test]
fn redirect_clobber() {
    let list = parse("echo >|foo").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![],
            redirections: vec![Redirection::File(FileRedirection {
                fd_number: 1,
                file_name: Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 7, 9),
                    remove_backslash: true,
                    splittable: true,
                }]),
                input: false,
                output: true,
                clobber: true,
                append: false,
            })],
            words: vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("echo"),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: true
            }]),]
        }))
    );
}

#[test]
fn redirect_input() {
    let list = parse("echo <foo").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![],
            redirections: vec![Redirection::File(FileRedirection {
                fd_number: 0,
                file_name: Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("foo"),
                    span: Span::new_to(0, 6, 8),
                    remove_backslash: true,
                    splittable: true,
                }]),
                input: true,
                output: false,
                clobber: false,
                append: false,
            })],
            words: vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("echo"),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: true
            }]),]
        }))
    );
}

#[test]
fn redirect_stderr_stdout() {
    let list = parse("echo 2>&1").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![],
            redirections: vec![Redirection::Fd(FdDuplication {
                src_fd_number: 1,
                dest_fd_number: 2
            })],
            words: vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("echo"),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: true
            }]),]
        }))
    );
}

#[test]
fn redirect_dup_for_input() {
    let list = parse("echo 0<&1").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![],
            redirections: vec![Redirection::Fd(FdDuplication {
                src_fd_number: 1,
                dest_fd_number: 0
            })],
            words: vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("echo"),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: true
            }]),]
        }))
    );
}

#[test]
fn redirect_input_and_output() {
    let list = parse("echo <>file").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![],
            redirections: vec![Redirection::File(FileRedirection {
                fd_number: 0,
                file_name: Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("file"),
                    span: Span::new_to(0, 7, 10),
                    remove_backslash: true,
                    splittable: true,
                }]),
                input: true,
                output: true,
                clobber: false,
                append: false,
            })],
            words: vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("echo"),
                span: Span::new_to(0, 0, 3),
                splittable: true,
                remove_backslash: true
            }]),]
        }))
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
            Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("file"),
                span: Span::new_to(0, 7, 10),
                remove_backslash: true,
                splittable: true,
            }]),
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
            Token::Eof(Pos::new(0, 5)),
            ParseErrorContext::ExpectingRightParen
        )
    );
}

#[test]
fn subshell() {
    let list = parse("(echo)").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::Subshell(CompoundList {
            commands: vec![Command::from(CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![],
                words: vec![Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("echo"),
                    span: Span::new_to(0, 1, 4),
                    splittable: true,
                    remove_backslash: true
                }]),]
            }))]
        }))
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
                    file_name: Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("foo"),
                        span: Span::new_to(0, 7, 9),
                        remove_backslash: true,
                        splittable: true,
                    }]),
                    input: false,
                    output: true,
                    clobber: false,
                    append: false,
                })]
            }),
            command: CommandType::Subshell(CompoundList {
                commands: vec![Command::from(CommandType::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    redirections: vec![],
                    words: vec![Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("echo"),
                        span: Span::new_to(0, 1, 4),
                        splittable: true,
                        remove_backslash: true
                    }]),]
                }))]
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
        Command::from(CommandType::SimpleCommand(SimpleCommand {
            assignments: vec![],
            redirections: vec![],
            words: vec![Token::Word(vec![WordComponent {
                kind: WordComponentKind::literal("{echo}"),
                span: Span::new_to(0, 0, 5),
                splittable: true,
                remove_backslash: true
            }]),]
        }))
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
            Token::Eof(Pos::new(0, 6)),
            ParseErrorContext::ExpectingRightBrace
        )
    );
}

#[test]
fn brace_group() {
    let list = parse("{ echo }").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::BraceGroup(CompoundList {
            commands: vec![Command::from(CommandType::SimpleCommand(SimpleCommand {
                assignments: vec![],
                redirections: vec![],
                words: vec![Token::Word(vec![WordComponent {
                    kind: WordComponentKind::literal("echo"),
                    span: Span::new_to(0, 2, 5),
                    splittable: true,
                    remove_backslash: true
                }]),]
            }))]
        }))
    );
}

#[test]
fn brace_group_sep_list() {
    let list = parse("{ echo ; boo }").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::BraceGroup(CompoundList {
            commands: vec![
                Command::from(CommandType::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    redirections: vec![],
                    words: vec![Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("echo"),
                        span: Span::new_to(0, 2, 5),
                        splittable: true,
                        remove_backslash: true
                    }]),]
                })),
                Command::from(CommandType::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    redirections: vec![],
                    words: vec![Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("boo"),
                        span: Span::new_to(0, 9, 11),
                        splittable: true,
                        remove_backslash: true
                    }]),]
                })),
            ]
        }))
    );
}

#[test]
fn brace_group_sep_newlien() {
    let list = parse("{\n\techo\n\tboo\n}").unwrap();
    assert_eq!(
        list,
        Command::from(CommandType::BraceGroup(CompoundList {
            commands: vec![
                Command::from(CommandType::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    redirections: vec![],
                    words: vec![Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("echo"),
                        span: Span::new_to(1, 1, 4),
                        splittable: true,
                        remove_backslash: true
                    }]),]
                })),
                Command::from(CommandType::SimpleCommand(SimpleCommand {
                    assignments: vec![],
                    redirections: vec![],
                    words: vec![Token::Word(vec![WordComponent {
                        kind: WordComponentKind::literal("boo"),
                        span: Span::new_to(2, 1, 3),
                        splittable: true,
                        remove_backslash: true
                    }]),]
                })),
            ]
        }))
    );
}
