(** Error and information reporting *)

val infof :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  ?span:Trace.span ->
  string -> ('a, Format.formatter, unit, unit) format4 -> 'a

val warnf :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  ?span:Trace.span ->
  string -> ('a, Format.formatter, unit, unit) format4 -> 'a

val debugf :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  ?span:Trace.span ->
  ('a, Format.formatter, unit, unit) format4 -> 'a

val enter : 
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?args:(string * (Format.formatter -> unit)) list ->
  string -> (Trace.span -> 'a) -> 'a

val (let@@) : ('a -> 'b) -> 'a -> 'b
val d : (Format.formatter -> 'a -> unit) -> 'a -> Format.formatter -> unit

(** [fail ~loc "msg"] raises a built-in generic failure, for convenience. *)
val fail :
  ?loc:Location.loc ->
  ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Levels *)

type level = Quiet | Normal | Debug
val level : level ref

type msg = Format.formatter -> unit

type report_kind =
  | Error
  | Warning of string
  | Info of string
  | Debug

type report = {
  kind : report_kind;
  loc : Location.loc;
  msg : msg;
  sub : Location.loc list;
}

val errorf :
  ?loc:Location.loc ->
  ?sub:Location.loc list ->
  ('a, Format.formatter, unit, report) format4 -> 'a

val register_report_of_exn : (exn -> report option) -> unit

val report_exception : Format.formatter -> exn -> unit

