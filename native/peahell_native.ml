open Peahell

module Make (L : Language) = struct

  module History = struct
    let filename = Sys.getenv "HOME" ^ "/." ^ L.name ^ ".history"

    let load () = ignore (LNoise.history_load ~filename)

    (* let res = function Ok x -> x | Error s -> error "%s" s *)
    let add s =
      LNoise.history_add s |> ignore ;
      LNoise.history_save ~filename |> ignore ;
  end


  (** Should the interactive shell be run? *)
  type mode = Batch | Interactive | Expect
  let mode = ref Interactive

  (** Trace printing *)
  type trace_out = Stdout | File of string
  let trace_out = ref (File "trace.json")

  (** The usage message. *)
  let usage =
    match L.file_parser with
    | Some _ -> "Usage: " ^ L.name ^ " [option] ... [file] ..."
    | None   -> "Usage:" ^ L.name ^ " [option] ..."

  (** A list of files to be loaded and run. *)
  let files = ref []

  (** Add a file to the list of files to be loaded, and record whether it should
      be processed in interactive mode. *)
  let add_file filename = (files := filename :: !files)

  (** Command-line options *)
  let options = Arg.align ([
      ("-v",
       Arg.Unit (fun () ->
           print_endline (L.name ^ " " ^ "(" ^ Sys.os_type ^ ")");
           exit 0),
       " Print language information and exit");
      ("-m",
       Arg.Symbol (["top";"batch";"expect"],
                   function "top" -> mode := Interactive
                          | "expect" ->  mode := Expect
                          | "batch" ->  mode := Batch
                          | _ -> ()
                  ),
       " Mode for running");
      ("-l",
       Arg.String (fun str -> add_file str),
       "<file> Load <file> into the initial environment");
      ("-q",
       Arg.Unit (fun () -> Report.level := Quiet),
       "Do not output anything");
      ("-debug",
       Arg.Unit (fun () -> Report.level := Debug),
       "Enable trace debuging");
      ("-trace",
       Arg.String (function
           | "stdout" -> trace_out := Stdout
           | s -> trace_out := File s),
       "Where to emit debug trace. Can be 'stdout' (for tracing to the stdout), or a filename. Default is 'trace.json'")
    ] @
      L.options)

  (** Treat anonymous arguments as files to be run. *)
  let anonymous str =
    add_file str

  (** Parse the contents from a file, using a given [parser]. *)
  let read_file parser fn =
    let fh = open_in fn in
    let lex = Lexing.from_channel fh in
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
    try
      let terms = Input.wrap (parser fn) lex in
      close_in fh;
      terms
    with
    (* Close the file in case of any parsing errors. *)
      err -> close_in fh ; raise err

  (** Parse input from toplevel, using the given [parser]. *)
  let read_toplevel parser () =
    let prompt = L.name ^ "> "
    and prompt_more = String.make (String.length L.name) ' ' ^ "> " in
    match LNoise.linenoise prompt with
    | None -> exit 0
    | Some s0 ->
      History.add s0;
      let rec aux acc =
        if L.read_more acc then match LNoise.linenoise prompt_more with
          | None -> exit 0
          | Some s ->
            History.add s;
            aux (acc ^ s)
        else begin
          parser @@ Lexing.from_string (acc ^ "\n")
        end
      in
      aux s0

  (** Load directives from the given file. *)
  let rec use_file ctx filename =
    match L.file_parser with
    | Some f ->
      let cmds = read_file f filename in
      L.exec use_file ctx cmds
    | None ->
      Report.fail "Cannot load files, only interactive shell is available"

  let batch ctx filenames =
    List.fold_left use_file ctx filenames

  (** Expect mode *)
  let expect ctx filename =
    match L.expect_parser with
    | Some (open_string, end_string, f) ->
      let ic = open_in filename in
      let l = read_file f filename in
      let rec walk_sections prev_index ctx = function
        | [] -> print_newline ()
        | (input, index1, index2) :: t ->
          seek_in ic prev_index;
          let txt = really_input_string ic (index1 - prev_index) in
          print_endline txt;
          print_endline open_string;
          let ctx =
            try L.exec use_file ctx input
            with err ->
              Format.printf "%a" Report.report_exception err;
              ctx
          in
          Format.print_flush ();
          print_newline ();
          print_string end_string;
          walk_sections index2 ctx t
      in
      walk_sections 0 ctx l
    | None ->
      Report.fail "I'm sorry, this language does not support expect mode"
  
  (** Interactive toplevel *)
  let toplevel ctx =
    let eof = match Sys.os_type with
      | "Unix" | "Cygwin" -> "Ctrl-D"
      | "Win32" -> "Ctrl-Z"
      | _ -> "EOF"
    in
    let toplevel_parser =
      match L.toplevel_parser with
      | Some p -> p
      | None ->
        Report.fail "I am sorry but this language has no interactive toplevel."
    in
    Format.printf "The %s interactive toplevel@." L.name ;
    Format.printf "Type %s to exit@." eof ;
    try
      let ctx = ref ctx in
      while true do
        try
          let cmd = read_toplevel (Input.wrap toplevel_parser) () in
          ctx := L.exec use_file !ctx cmd
        with
        | Sys.Break -> prerr_endline "Interrupted."
        | err -> Format.eprintf "%a@." Report.report_exception err
      done
    with End_of_file -> ()

  let run f =
    begin match !Report.level, !trace_out with
      | Debug, Stdout -> Trace_tef.setup ~out:`Stdout ()
      | Debug, File s -> Trace_tef.setup ~out:(`File s) ()
      | _ -> ()
    end;
    try
      f ();
      Trace.shutdown ();
      exit 0
    with err ->
      Format.eprintf "%a@." Report.report_exception err;
      Trace.shutdown ();
      exit 1 
  
  (** Main program *)
  let main () =
    LNoise.set_multiline true;
    History.load () ;
    (* Intercept Ctrl-C by the user *)
    LNoise.catch_break true;
    (* Parse the arguments. *)
    Arg.parse options anonymous usage;
    (* Files were listed in the wrong order, so we reverse them *)
    files := List.rev !files;
    (* Set the maximum depth of pretty-printing, after which it prints ellipsis. *)
    Format.set_max_boxes 42 ;
    Format.set_ellipsis_text "..." ;
    Format.set_margin 80 ;
    Format.set_max_indent 30 ;
    (* Run and load all the specified files. *)
    run begin fun () ->
      match !mode with
      | Batch ->
        let _ = batch L.initial_environment !files in
        ()
      | Interactive -> 
        let ctx = batch L.initial_environment !files in
        toplevel ctx
      | Expect ->
        let file = match !files with
          | [fn] -> fn
          | _ -> Report.fail "Expect mode only accept one file"
        in
        expect L.initial_environment file
    end 
end
