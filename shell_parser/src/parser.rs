use crate::types::*;
use failure::{bail, ensure, Error, Fail, Fallible};
use shell_lexer::{Lexer, Operator, ReservedWord, Token};
use std::io::Read;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorContext {
    List,
    SimpleCommand,
    PipelineStartingWithBang,
    PipeSequence,
    IoFileAfterIoNumber,
    FileNameAfterRedirectionOperator,
    ExpectingPipelineAfter(Operator),
    FdRedirectionExpectsNumber,
    ExpectingRightBrace,
    ExpectingRightParen,
}

#[derive(Debug, Clone, PartialEq, Eq, Fail)]
pub enum ParseErrorKind {
    #[fail(display = "Unexpected token {:?} while parsing {:?}", 0, 1)]
    UnexpectedToken(Token, ParseErrorContext),
}

pub struct Parser<R: Read> {
    lexer: Lexer<R>,
    lookahead: Option<Token>,
}

impl<R: Read> Parser<R> {
    pub fn new(stream: R) -> Self {
        let lexer = Lexer::new(stream);
        Self {
            lexer,
            lookahead: None,
        }
    }

    /// Main entry point to the parser; parses a program
    pub fn parse(&mut self) -> Fallible<Command> {
        self.program()
    }
}

impl<R: Read> Parser<R> {
    fn unexpected_next_token(&mut self, context: ParseErrorContext) -> Error {
        match self.next_token() {
            Ok(tok) => ParseErrorKind::UnexpectedToken(tok, context).into(),
            Err(e) => e,
        }
    }

    /// If the next token is an operator with a kind that matches
    /// any of those in the candidates slice, get that token and
    /// return it.  Otherwise returns None.
    fn next_token_is_operator(&mut self, candidates: &[Operator]) -> Fallible<Option<Token>> {
        let tok = self.next_token()?;
        if let Token::Operator(operator, ..) = &tok {
            for op in candidates {
                if op == operator {
                    return Ok(Some(tok));
                }
            }
        }
        self.unget_token(tok);
        Ok(None)
    }

    /// If the next token is the requested reserved word, consume it
    /// and return true.  Otherwise, put it back and return false.
    fn next_token_is_reserved_word(&mut self, reserved: ReservedWord) -> Fallible<bool> {
        let tok = self.next_token()?;
        if tok.is_reserved_word(reserved) {
            return Ok(true);
        }
        self.unget_token(tok);
        Ok(false)
    }

    /// Consume the next token
    fn next_token(&mut self) -> Fallible<Token> {
        if let Some(tok) = self.lookahead.take() {
            Ok(tok)
        } else {
            self.lexer.next_token()
        }
    }

    /// Place a token into the lookahead.
    /// The lookahead must be vacant, or else this will cause
    /// a panic; we only support a single lookahead.
    fn unget_token(&mut self, tok: Token) {
        assert!(self.lookahead.is_none());
        self.lookahead.replace(tok);
    }
}

impl<R: Read> Parser<R> {
    /// Parse the top level program syntax, a potentially empty list
    /// of child commands.
    fn program(&mut self) -> Fallible<Command> {
        let mut cmd = match self.and_or()? {
            Some(cmd) => cmd,
            None => {
                let tok = self.next_token()?;
                if let Token::Eof(..) = &tok {
                    // We parsed an empty program
                    return Ok(CommandType::SimpleCommand(Default::default()).into());
                }
                return Err(self.unexpected_next_token(ParseErrorContext::List));
            }
        };
        cmd.asynchronous = self.separator_is_async()?;
        let mut commands = vec![cmd];

        while let Some(mut cmd) = self.and_or()? {
            cmd.asynchronous = self.separator_is_async()?;
            commands.push(cmd);
        }

        if commands.len() == 1 {
            Ok(commands.pop().unwrap())
        } else {
            let is_async = commands.last().unwrap().asynchronous;

            let mut command: Command = CommandType::Program(CompoundList { commands }).into();
            command.asynchronous = is_async;
            Ok(command)
        }
    }

    fn and_or(&mut self) -> Fallible<Option<Command>> {
        if let Some(pipeline) = self.pipeline()? {
            match self.next_token_is_operator(&[Operator::AndIf, Operator::OrIf])? {
                Some(Token::Operator(operator, ..)) => {
                    self.pipeline_conditional(pipeline, operator)
                }
                _ => Ok(Some(pipeline.into())),
            }
        } else {
            Ok(None)
        }
    }

    fn pipeline_conditional(
        &mut self,
        condition: Pipeline,
        op: Operator,
    ) -> Fallible<Option<Command>> {
        self.linebreak()?;

        let then: CompoundList = Command::from(self.pipeline()?.ok_or_else(|| {
            self.unexpected_next_token(ParseErrorContext::ExpectingPipelineAfter(op))
        })?)
        .into();
        let condition: CompoundList = Command::from(condition).into();

        let (true_part, false_part) = if op == Operator::AndIf {
            (Some(then), None)
        } else {
            (None, Some(then))
        };

        Ok(Some(
            CommandType::If(If {
                condition: condition.into(),
                true_part,
                false_part,
            })
            .into(),
        ))
    }

    fn pipeline(&mut self) -> Fallible<Option<Pipeline>> {
        let inverted = self.next_token_is_reserved_word(ReservedWord::Bang)?;
        if let Some(commands) = self.pipe_sequence()? {
            Ok(Some(Pipeline { inverted, commands }))
        } else if inverted {
            Err(self.unexpected_next_token(ParseErrorContext::PipelineStartingWithBang))
        } else {
            Ok(None)
        }
    }

    fn pipe_sequence(&mut self) -> Fallible<Option<Vec<Command>>> {
        let command = match self.command()? {
            None => return Ok(None),
            Some(cmd) => cmd,
        };

        let mut commands = vec![command];

        while self.next_token_is_operator(&[Operator::Pipe])?.is_some() {
            self.linebreak()?;
            match self.command()? {
                Some(cmd) => commands.push(cmd),
                None => return Err(self.unexpected_next_token(ParseErrorContext::PipeSequence)),
            }
        }

        for cmd in commands.iter_mut().rev().skip(1) {
            cmd.asynchronous = true;
        }

        Ok(Some(commands))
    }

    fn command(&mut self) -> Fallible<Option<Command>> {
        if let Some(cmd) = self.compound_command()? {
            Ok(Some(cmd))
        } else if let Some(group) = self.subshell()? {
            Ok(Some(Command {
                command: CommandType::Subshell(group),
                asynchronous: false,
                redirects: None,
            }))
        } else if let Some(command) = self.simple_command()? {
            Ok(Some(Command {
                command: CommandType::SimpleCommand(command),
                asynchronous: false,
                redirects: None,
            }))
        } else {
            // TODO: function_definition
            Ok(None)
        }
    }

    fn compound_command(&mut self) -> Fallible<Option<Command>> {
        let mut command = if let Some(group) = self.brace_group()? {
            Command {
                command: CommandType::BraceGroup(group),
                asynchronous: false,
                redirects: None,
            }
        } else if let Some(group) = self.subshell()? {
            Command {
                command: CommandType::Subshell(group),
                asynchronous: false,
                redirects: None,
            }
        } else {
            // TODO: for_clause, case_clause, if_clause, while_clause, until_clause
            return Ok(None);
        };

        command.redirects = self.redirect_list()?;
        Ok(Some(command))
    }

    fn subshell(&mut self) -> Fallible<Option<CompoundList>> {
        if self
            .next_token_is_operator(&[Operator::LeftParen])?
            .is_none()
        {
            return Ok(None);
        }

        let list = self.compound_list()?;

        if self
            .next_token_is_operator(&[Operator::RightParen])?
            .is_some()
        {
            Ok(Some(list))
        } else {
            Err(self.unexpected_next_token(ParseErrorContext::ExpectingRightParen))
        }
    }

    fn compound_list(&mut self) -> Fallible<CompoundList> {
        let mut commands = vec![];

        loop {
            self.newline_list()?;

            if let Some(cmd) = self.and_or()? {
                commands.push(cmd);
            } else {
                break;
            }

            self.separator()?;
        }

        Ok(CompoundList { commands })
    }

    fn brace_group(&mut self) -> Fallible<Option<CompoundList>> {
        if !self.next_token_is_reserved_word(ReservedWord::LeftBrace)? {
            return Ok(None);
        }

        let list = self.compound_list()?;

        if self.next_token_is_reserved_word(ReservedWord::RightBrace)? {
            Ok(Some(list))
        } else {
            Err(self.unexpected_next_token(ParseErrorContext::ExpectingRightBrace))
        }
    }

    fn redirect_list(&mut self) -> Fallible<Option<RedirectList>> {
        let mut redirections = vec![];
        loop {
            if let Some(redir) = self.io_redirect()? {
                redirections.push(redir);
            } else if redirections.is_empty() {
                return Ok(None);
            } else {
                return Ok(Some(RedirectList { redirections }));
            }
        }
    }

    fn io_redirect(&mut self) -> Fallible<Option<Redirection>> {
        let t = self.next_token()?;
        if let Token::IoNumber(fd_number, ..) = &t {
            match self.io_file(Some(*fd_number))? {
                Some(redir) => return Ok(Some(redir)),
                None => {
                    return Err(self.unexpected_next_token(ParseErrorContext::IoFileAfterIoNumber));
                }
            }
        }
        self.unget_token(t);
        self.io_file(None)
    }

    fn io_file(&mut self, fd_number: Option<usize>) -> Fallible<Option<Redirection>> {
        let t = self.next_token()?;
        let oper = if let Token::Operator(oper, ..) = &t {
            match oper {
                Operator::Less
                | Operator::LessAnd
                | Operator::Great
                | Operator::GreatAnd
                | Operator::DoubleGreat
                | Operator::LessGreat
                | Operator::Clobber => oper,
                _ => {
                    self.unget_token(t);
                    return Ok(None);
                }
            }
        } else {
            self.unget_token(t);
            return Ok(None);
        };

        match oper {
            Operator::GreatAnd | Operator::LessAnd => {
                if let Some(src_fd_number) = self.number()? {
                    let dest_fd_number =
                        fd_number.unwrap_or(if *oper == Operator::GreatAnd { 1 } else { 0 });
                    return Ok(Some(Redirection::Fd(FdDuplication {
                        src_fd_number,
                        dest_fd_number,
                    })));
                } else {
                    return Err(
                        self.unexpected_next_token(ParseErrorContext::FdRedirectionExpectsNumber)
                    );
                }
            }
            _ => {}
        }

        let file_name = self.next_token()?;
        if let Token::Word(_) = &file_name {
            Ok(Some(match oper {
                Operator::Less => Redirection::File(FileRedirection {
                    fd_number: fd_number.unwrap_or(0),
                    file_name,
                    input: true,
                    output: false,
                    clobber: false,
                    append: false,
                }),
                Operator::LessGreat => Redirection::File(FileRedirection {
                    fd_number: fd_number.unwrap_or(0),
                    file_name,
                    input: true,
                    output: true,
                    clobber: false,
                    append: false,
                }),
                Operator::Great => Redirection::File(FileRedirection {
                    fd_number: fd_number.unwrap_or(1),
                    file_name,
                    input: false,
                    output: true,
                    clobber: false,
                    append: false,
                }),
                Operator::DoubleGreat => Redirection::File(FileRedirection {
                    fd_number: fd_number.unwrap_or(1),
                    file_name,
                    input: false,
                    output: true,
                    clobber: false,
                    append: true,
                }),
                Operator::Clobber => Redirection::File(FileRedirection {
                    fd_number: fd_number.unwrap_or(1),
                    file_name,
                    input: false,
                    output: true,
                    clobber: true,
                    append: false,
                }),
                _ => bail!("impossible redirection oper {:?}", oper),
            }))
        } else {
            self.unget_token(file_name);
            Err(self.unexpected_next_token(ParseErrorContext::FileNameAfterRedirectionOperator))
        }
    }

    fn simple_command(&mut self) -> Fallible<Option<SimpleCommand>> {
        let mut assignments = vec![];
        let mut words = vec![];
        let mut redirections = vec![];

        loop {
            if let Some(redir) = self.io_redirect()? {
                redirections.push(redir);
                continue;
            }

            let token = self.next_token()?;
            match &token {
                Token::Assignment(assign) => {
                    ensure!(
                        words.is_empty(),
                        "FIXME: interpret in non-assignment context"
                    );
                    assignments.push(assign.clone());
                }
                Token::Word(_) => {
                    if token.is_reserved_word(ReservedWord::RightBrace) {
                        self.unget_token(token);
                        break;
                    }
                    words.push(token);
                }

                _ => {
                    self.unget_token(token);
                    break;
                }
            }
        }

        if assignments.is_empty() && words.is_empty() && redirections.is_empty() {
            return Ok(None);
        }

        Ok(Some(SimpleCommand {
            assignments,
            redirections,
            words,
        }))
    }
}

impl<R: Read> Parser<R> {
    /// Consumes an optional sequence of newline tokens.
    fn linebreak(&mut self) -> Fallible<()> {
        self.newline_list()?;
        Ok(())
    }

    /// Consumes a sequence of newline tokens.
    /// Returns true if any newline tokens were seen,
    /// false if none were seen.
    fn newline_list(&mut self) -> Fallible<bool> {
        let mut seen = false;
        loop {
            let t = self.next_token()?;
            if let Token::Newline(..) = &t {
                seen = true;
            } else {
                self.unget_token(t);
                return Ok(seen);
            }
        }
    }

    /// Parse a decimal number.
    /// This is used in fd redirection for constructs
    /// like `1>&1`.  The first number is recognized as
    /// an IoNumber and we are called for the second number.
    fn number(&mut self) -> Fallible<Option<usize>> {
        let t = self.next_token()?;
        if let Some(word) = t.as_single_literal_word_string() {
            if let Ok(num) = usize::from_str_radix(word, 10) {
                return Ok(Some(num));
            }
        }
        self.unget_token(t);
        Ok(None)
    }

    /// Matches a single `;` or `&` separator operator
    fn separator_op(&mut self) -> Fallible<Option<Separator>> {
        match self.next_token_is_operator(&[Operator::Semicolon, Operator::Ampersand])? {
            Some(Token::Operator(Operator::Semicolon, ..)) => Ok(Some(Separator::Sync)),
            Some(Token::Operator(Operator::Ampersand, ..)) => Ok(Some(Separator::Async)),
            _ => Ok(None),
        }
    }

    /// Matches either an explicit separator operator or an implicit
    /// synchronous separator in the form of a newline.
    fn separator(&mut self) -> Fallible<Option<Separator>> {
        if let Some(sep) = self.separator_op()? {
            self.linebreak()?;
            Ok(Some(sep))
        } else if self.newline_list()? {
            Ok(Some(Separator::Sync))
        } else {
            Ok(None)
        }
    }

    /// Matches an optional separator, returning true if that separator
    /// is async, or false if it is sync or not present.
    fn separator_is_async(&mut self) -> Fallible<bool> {
        Ok(self.separator()?.unwrap_or(Separator::Sync) == Separator::Async)
    }
}

#[derive(PartialEq, Eq)]
enum Separator {
    Sync,
    Async,
}
