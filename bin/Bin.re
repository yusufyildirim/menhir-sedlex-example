open Lib;

let _ = {
  let input = "12 + 5 * 2";

  let result = Parser.parse(input);

  switch result {
  | Ok(result) => Console.log(("Calculation result :", result))
  | Error(`LexError(e)) => Console.log(("LexError", e))
  | Error(`ParseError(e)) => Console.log(("ParserError", e))
  };
}