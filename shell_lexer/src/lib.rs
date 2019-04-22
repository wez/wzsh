mod errors;
mod lexer;
mod position;
mod reader;
#[macro_use]
mod tokenenum;

pub use errors::LexErrorKind;
pub use lexer::{Assignment, Lexer, ParamExpr, ParamOper, Token, WordComponent, WordComponentKind};
pub use position::{Pos, Span};
pub use reader::CharReader;
pub use tokenenum::LiteralMatcher;

TokenEnum!(
    Operator,
    OPERATORS,
    "<<-": DoubleLessDash,
    "<<": DoubleLess,
    "<&": LessAnd,
    "<>": LessGreat,
    ">>": DoubleGreat,
    ">|": Clobber,
    ">&": GreatAnd,
    "&&": AndIf,
    "||": OrIf,
    ";;": DoubleSemicolon,
    "<": Less,
    "&": Ampersand,
    "|": Pipe,
    ";": Semicolon,
    ">": Great,
    "(": LeftParen,
    ")": RightParen
);

TokenEnum!(
    ReservedWord,
    RESERVED_WORDS,
    "if": If,
    "then": Then,
    "else": Else,
    "elif": Elif,
    "fi": Fi,
    "do": Do,
    "done": Done,
    "case": Case,
    "esac": Esac,
    "while": While,
    "until": Until,
    "for": For,
    "{": LeftBrace,
    "}": RightBrace,
    "!": Bang,
    "in": In
);
