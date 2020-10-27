(** Source code locations. *)
type loc = 
  | File of string
  | Location of Lexing.position * Lexing.position (** delimited location *)
  | Nowhere (** no location *)

(** A datum tagged with a source code location *)
type 'a t = { data : 'a ; loc : loc }

(** Tag a datum with an (optional) location. *)
val mk : ?loc:loc -> 'a -> 'a t

(** Convert a [Lexing.lexbuf] location to a [location] *)
val of_lex : Lexing.lexbuf -> loc

(** [loc p1 p2] creates a location which starts at [p1] and ends at [p2]. *)
val loc : Lexing.position -> Lexing.position -> loc

(** [noloc] is [Nowhere] *)
val noloc : loc

(** Print a location *)
val pp : Format.formatter -> loc -> unit

(** Currently considered file *)
val input_file : string option ref
val input_file_as_loc : unit -> loc
