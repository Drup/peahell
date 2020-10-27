type filename = string

module type S = sig
  val name : string
  type command
  type environment
  val options : (Arg.key * Arg.spec * Arg.doc) list
  val initial_environment : environment
  val read_more : string -> bool
  val file_parser : (Lexing.lexbuf -> command list) option
  val toplevel_parser : (Lexing.lexbuf -> command) option
  val exec :
    (environment -> filename -> environment) ->
    environment -> command -> environment
end
