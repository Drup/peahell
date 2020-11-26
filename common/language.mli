type filename = string

module type S = sig
  val name : string
  type input
  type environment
  val options : (Arg.key * Arg.spec * Arg.doc) list
  val initial_environment : environment
  val read_more : string -> bool
  val file_parser : (string -> Lexing.lexbuf -> input) option
  val toplevel_parser : (Lexing.lexbuf -> input) option
  val expect_parser :
    (string * string * (string -> Lexing.lexbuf -> (input * int * int) list)) option
  val exec :
    Format.formatter ->
    (environment -> filename -> environment) ->
    environment -> input -> environment
end
