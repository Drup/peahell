(** Error and information reporting *)

val printf :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  ('a, Format.formatter, unit, unit) format4 -> 'a

val eprintf :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  ('a, Format.formatter, unit, unit) format4 -> 'a

val fprintf :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  Format.formatter ->
  ('a, Format.formatter, unit, unit) format4 -> 'a

val infof :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  string -> ('a, Format.formatter, unit, unit) format4 -> 'a

val warnf :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  string -> ('a, Format.formatter, unit, unit) format4 -> 'a

type msg = (Format.formatter -> unit) Location.t

type report_kind =
  | Error
  | Warning of string
  | Info of string
  | Output

type report = {
  kind : report_kind;
  main : msg;
  sub : Location.loc list;
}

val errorf :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  ('a, Format.formatter, unit, report) format4 -> 'a

val register_report_of_exn : (exn -> report option) -> unit

val report_exception : Format.formatter -> exn -> unit

(** [fail ~loc "msg"] raises a built-in generic failure, for convenience. *)
val fail :
  ?loc:Location.loc ->
  ('a, Format.formatter, unit, 'b) format4 -> 'a
