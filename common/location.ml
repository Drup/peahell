type loc =
  | File of string
  | Location of Lexing.position * Lexing.position (** delimited location *)
  | Nowhere (** no location *)

type 'a t = { data : 'a ; loc : loc }

let loc loc1 loc2 = Location (loc1, loc2)
let noloc = Nowhere

let of_lex lex =
  Location (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

let mk ?(loc=Nowhere) x = { data = x; loc = loc }

let pp ppf loc =
  match loc with
  | Nowhere ->
    Format.fprintf ppf "unknown location"
  | File filename ->
    Format.fprintf ppf "file %S" filename
  | Location (begin_pos, end_pos) ->
    let begin_char = begin_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
    let end_char = end_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
    let begin_line = begin_pos.Lexing.pos_lnum in
    let filename = begin_pos.Lexing.pos_fname in
    if String.length filename != 0 then
      Format.fprintf ppf "file %S, line %d, charaters %d-%d"
        filename begin_line begin_char end_char
    else
      Format.fprintf ppf "line %d, characters %d-%d"
        (begin_line - 1) begin_char end_char

let input_file : string option ref = ref None
let input_file_as_loc () =
  match !input_file with
  | None -> Nowhere
  | Some f -> File f
