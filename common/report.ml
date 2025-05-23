open Location

type level =
  | Quiet
  | Normal
  | Debug
let level = ref Normal

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

type report_printer = {
  (* The entry point *)
  pp : report_printer ->
    Format.formatter -> report -> unit;
  pp_report_kind : report_printer -> report ->
    Format.formatter -> report_kind -> unit;
  pp_main_loc : report_printer -> report ->
    Format.formatter -> loc -> unit;
  pp_main_txt : report_printer -> report ->
    Format.formatter -> (Format.formatter -> unit) -> unit;
  out : Format.formatter ;
  err : Format.formatter ;
}

let batch_mode_printer : report_printer =
  let out = Format.std_formatter in
  let err = Format.err_formatter in
  let pp_loc _self _report ppf loc =
    Format.fprintf ppf "@[%a:@]" Location.pp loc
  in
  let pp_txt ppf txt = Format.fprintf ppf "@[%t@]" txt in
  let pp self ppf ({ kind; msg ; loc ; sub } as report) =
    match loc with
    | Location.Nowhere ->
      Format.fprintf ppf "@[<v>%a%a@,%a@]@."
        (self.pp_report_kind self report) kind
        (self.pp_main_txt self report) msg
        (Fmt.list ~sep:Fmt.cut @@ self.pp_main_loc self report) sub
    | loc ->
      Format.fprintf ppf "@[<v>%a@ %a%a@,%a@]@."
        (self.pp_main_loc self report) loc
        (self.pp_report_kind self report) kind
        (self.pp_main_txt self report) msg
        (Fmt.list ~sep:Fmt.cut @@ self.pp_main_loc self report) sub
  in
  let pp_report_kind _self _ ppf = function
    | Error -> Format.fprintf ppf "@{<error>[Error]@}:@ "
    | Warning w -> Format.fprintf ppf "@{<warning>[Warning@} %s]:@ " w
    | Info w -> Format.fprintf ppf "@{<info>[%s]@}:@ " w
    | Debug -> ()
  in
  let pp_main_loc self report ppf loc =
    pp_loc self report ppf loc
  in
  let pp_main_txt _self _ ppf txt =
    pp_txt ppf txt
  in
  { pp; pp_report_kind; pp_main_loc; pp_main_txt ; out ; err }

let report_printer = ref batch_mode_printer

let pp_report ppf report =
  let printer = !report_printer in
  printer.pp printer ppf report

(* Register *)

let report_of_exn : (exn -> report option) list ref = ref []

let register_report_of_exn f = report_of_exn := f :: !report_of_exn

let report_of_exn exn =
  let rec loop = function
    | [] -> None
    | f :: rest ->
      match f exn with
      | Some error -> Some error
      | None -> loop rest
  in
  loop !report_of_exn

external reraise : exn -> 'a = "%reraise"
let report_exception ppf exn =
  let rec loop n exn =
    match report_of_exn exn with
    | None -> reraise exn
    | Some r ->
      pp_report ppf r;
      if !level = Debug then Printexc.print_backtrace stdout
    | exception exn when n > 0 -> loop (n-1) exn
  in
  loop 5 exn

let () = Printexc.record_backtrace true

(** Smart constructors *)

let errorf ?(loc = Nowhere) ?(sub = []) =
  Format.kdprintf (fun msg -> { kind = Error; loc; msg ; sub})

let outf ?(loc = Nowhere) ?(sub = []) ?span kind =
  Format.kdprintf (fun msg ->
      if !level = Quiet then () else
        pp_report !report_printer.out { kind; loc; msg; sub};
      Trace.messagef ?span
        (fun k -> k "%a"
            pp_report { kind; loc; msg; sub})
    )

let warnf ?loc ?sub ?span w = outf ?loc ?sub ?span (Warning w)
let infof ?loc ?sub ?span i = outf ?loc ?sub ?span (Info i)

let debugf ?(loc = Nowhere) ?(sub = []) ?span = 
  Format.kdprintf (fun msg ->
      begin if !level = Debug then
          pp_report !report_printer.out { kind=Debug; loc; msg; sub}
      end;
      Trace.messagef ?span
        (fun k -> k "%a"
            pp_report { kind = Debug; loc; msg; sub})
    )

let enter ?__FUNCTION__ ~__FILE__ ~__LINE__ ?(args=[]) s f =
  let data () =
    List.map (fun (s, t) -> s, `String (Format.asprintf "%t" t)) args
  in
  Trace.with_span ?__FUNCTION__ ~__FILE__ ~__LINE__ ~data s f
let d pp x = Format.dprintf "%a" pp x
let (let@@) = (@@)

(** A builtin error, for convenience *)

exception Fail of (loc * msg)
let () = register_report_of_exn @@ function
  | Fail (loc, msg) ->
    Some (errorf ~loc "@[%t@]@." msg)
  | _ -> None
let fail ?(loc=Nowhere) = 
  let k msg = raise @@ Fail (loc, msg) in
  Format.kdprintf k

(** Generic errors *)

let () =
  register_report_of_exn @@ function
  | Sys_error msg ->
    Some (errorf ~loc:(Location.input_file_as_loc ()) "I/O error: %s" msg)
  | _ -> None

