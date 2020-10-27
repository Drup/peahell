module Lex : sig
  type error = ..
  exception Error of error * Location.loc

  type error +=
    | Illegal_character of char
end

module Parse : sig
  type error = ..
  exception Error of error

  type error +=
    | Unexpected of Location.loc
end

val wrap : (Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'a
