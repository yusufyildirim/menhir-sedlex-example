open Sedlexing;
open Calculation;
type lexingError = {
  pos: (Lexing.position, Lexing.position),
  lexeme: string,
};
exception LexError(lexingError);

let lexFail = lexbuf => {
  let pos = Sedlexing.lexing_positions(lexbuf);
  let char = Utf8.lexeme(lexbuf);
  raise @@ LexError({
    pos,
    lexeme: char,
  });
}

let digit = [%sedlex.regexp? '0'..'9'];
let number = [%sedlex.regexp? Plus(digit)];

let whitespace = [%sedlex.regexp ? ' ' | '\t']

let rec token = (lexbuf) =>
{
  switch%sedlex lexbuf {
    | whitespace => token(lexbuf)
    | '\n' => EOL
    | number => {
      INT(int_of_string(Utf8.lexeme(lexbuf)))
    }
    | '+' => PLUS
    | '-' => MINUS
    | '*' => TIMES
    | '/' => DIVIDE
    | '(' => LPARAN
    | ')' => RPARAN
    | eof => EOF
    | _ => lexFail(lexbuf)
  };
}

let make = lexbuf =>  Sedlexing.with_tokenizer(token, lexbuf);
let from_string = Sedlexing.Utf8.from_string