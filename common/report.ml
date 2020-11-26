open Location

type msg = (Format.formatter -> unit) t

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
}

let batch_mode_printer : report_printer =
  let pp_loc _self _report ppf loc =
    Format.fprintf ppf "@[<v>%a:@]" Location.pp loc
  in
  let pp_txt ppf txt = Format.fprintf ppf "@[%t@]" txt in
  let pp self ppf ({ kind; main; sub } as report) =
    match main.loc with
    | Location.Nowhere ->
      Format.fprintf ppf "@[<v>%a%a@,%a@]"
        (self.pp_report_kind self report) kind
        (self.pp_main_txt self report) main.data
        (Fmt.list ~sep:Fmt.cut @@ self.pp_main_loc self report) sub
    | loc ->
      Format.fprintf ppf "@[<v>%a@ %a%a@,%a@]"
        (self.pp_main_loc self report) loc
        (self.pp_report_kind self report) kind
        (self.pp_main_txt self report) main.data
        (Fmt.list ~sep:Fmt.cut @@ self.pp_main_loc self report) sub
  in
  let pp_report_kind _self _ ppf = function
    | Error -> Format.fprintf ppf "@{<error>Error@}: "
    | Warning w -> Format.fprintf ppf "@{<warning>Warning@} %s: " w
    | Info w -> Format.fprintf ppf "@{<info>Info@} %s: " w
    | Output -> ()
  in
  let pp_main_loc self report ppf loc =
    pp_loc self report ppf loc
  in
  let pp_main_txt _self _ ppf txt =
    pp_txt ppf txt
  in
  { pp; pp_report_kind; pp_main_loc; pp_main_txt }

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
    | Some r -> pp_report ppf r
    | exception exn when n > 0 -> loop (n-1) exn
  in
  loop 5 exn

let () = Printexc.record_backtrace true

(** Smart constructors *)

let errorf ?(loc = Nowhere) ?(sub = []) =
  Format.kdprintf (fun data -> { kind = Error; main = { loc; data } ; sub})

let warnf ?(loc = Nowhere) ?(sub = []) w =
  Format.kdprintf (fun data ->
      pp_report Fmt.stdout { kind = Warning w ; main = { loc; data } ; sub})

let infof ?(loc = Nowhere) ?(sub = []) i =
  Format.kdprintf (fun data ->
      pp_report Fmt.stdout { kind = Info i ; main = { loc; data } ; sub})

let printf ?(loc = Nowhere) ?(sub = []) =
  Format.kdprintf (fun data ->
      pp_report Fmt.stdout { kind = Output ; main = { loc; data } ; sub})

let fprintf ?(loc = Nowhere) ?(sub = []) ppf =
  Format.kdprintf (fun data ->
      pp_report ppf { kind = Output ; main = { loc; data } ; sub})

(** A builtin error, for convenience *)

exception Fail of (loc * string)
let () = register_report_of_exn @@ function
  | Fail (loc, msg) ->
    Some (errorf ~loc "Error: %s@." msg)
  | _ -> None
let fail ?(loc=Nowhere) = 
  let k _ = raise @@ Fail (loc, Format.flush_str_formatter ()) in
  Format.kfprintf k Format.str_formatter

(** Generic errors *)

let () =
  register_report_of_exn @@ function
  | Sys_error msg ->
    Some (errorf ~loc:(Location.input_file_as_loc ()) "I/O error: %s" msg)
  | _ -> None

