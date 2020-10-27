module Lex = struct

  type error = ..
  exception Error of error * Location.loc

  type error +=
    | Illegal_character of char

  let prepare_error = function
    | Error (Illegal_character c, loc) ->
      Some (Report.errorf ~loc "Illegal character (%s)" (Char.escaped c))
    | _ -> None
  let () = Report.register_report_of_exn prepare_error

end

module Parse = struct

  type error = ..
  exception Error of error

  type error +=
    | Unexpected of Location.loc

  let prepare_error = function
    | Error (Unexpected loc) ->
      Some (Report.errorf ~loc "Unexpected token")
    | _ -> None
  let () = Report.register_report_of_exn prepare_error

end

let wrap parse lexbuf =
  try
    parse lexbuf
  with
  | Parsing.Parse_error -> 
    let loc =
      Location.Location (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
    in
    raise(Parse.Error(Parse.Unexpected loc))
