module I = Calculation.MenhirInterpreter;
module S = MenhirLib.General;

let rec loop = (next_token, lexbuf, checkpoint: I.checkpoint(int)) => {
  switch checkpoint {
  | I.InputNeeded(_env) => {
    let token = next_token();
    let checkpoint = I.offer(checkpoint, token);
    loop(next_token, lexbuf, checkpoint);
  }
  | I.Shifting(_)
  | I.AboutToReduce(_) => {
    let checkpoint = I.resume(checkpoint);
    loop(next_token, lexbuf, checkpoint);
  }
  | I.HandlingError(env) => {
      // Printf.fprintf(stderr, "At offset %d: syntax error.\n%!", Sedlexing.lexeme_start(lexbuf));
      Error(`ParseError("Error"));
    }
  | I.Accepted(v) => Ok(v)
  | I.Rejected => Error(`ParseError("Rejected"))
  };
  }

let parse = (input) => {
  let lexbuf = Lexer.from_string(input);
  let lexer = Lexer.make(lexbuf);
  let checkpoint = Calculation.Incremental.calculate(fst @@ Sedlexing.lexing_positions(lexbuf));
  
  switch(loop(lexer, lexbuf, checkpoint)) {
  | result => result
  | exception Lexer.LexError(e) => Error(`LexError(e))
  }
}