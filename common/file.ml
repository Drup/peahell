let outfile_path = ref "."

type file_emitter =
  filename:string ->
  title:string ->
  mime:string option ->
  (Format.formatter -> unit) ->
  unit

let native_file_emitter ~filename ~title:_ ~mime:_ pf =
  Report.debugf "Emitting %s" filename;
  let s = !outfile_path ^ "/" ^ filename in
  let oc = open_out s in
  let fmt = Format.formatter_of_out_channel oc in
  Fmt.pf fmt "%t@." pf;
  close_out oc

let file_emitter : file_emitter ref = ref native_file_emitter

let emitf ?mime ~title ~filename fmt =
  Format.kdprintf (fun pf -> !file_emitter ~filename ~mime ~title pf) fmt

let debugf ?mime ~title ~filename fmt =
  Format.kdprintf (fun pf ->
      if (!Report.level = Debug) then
        !file_emitter ~filename ~mime ~title pf)
    fmt

class ['t]logger ?mime printer = object
  val mutable step_count = 0
  method app : 'a . _ -> _ -> ('a -> 't ) -> 'a -> _ = 
    fun filename title f x ->
    let filename = Fmt.str "%02d_%s" step_count filename in
    Report.debugf "Emitting %s" filename;
    let y = Report.enter ~__LINE__ ~__FILE__ title @@ fun _ -> f x in
    debugf ?mime ~title ~filename "%a" printer (title, y);
    step_count <- step_count + 1;
    y
end
