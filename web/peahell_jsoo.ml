open Peahell

module DomRepl = struct
  open Js_of_ocaml
  open Js_of_ocaml_tyxml.Tyxml_js

  let set_lang_name name =
    Dom_html.document##.title := Js.string name;
    Register.id ~keep:false "lang" [Html.txt name]

  let load_file s : unit =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "loadfile")
      [|Js.Unsafe.inject @@ Js.string s|]

  let clear_term () : unit = 
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "clear_term")
      [||]

  let add_to_term s : unit =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "add_to_term")
      [|Js.Unsafe.inject @@ Js.string s|]
  let flush_term () : unit =
    Js.Unsafe.fun_call (Js.Unsafe.js_expr "flush_term")
      [||]

  let term =
    let t = Format.make_formatter
        (fun s pos len ->
           let s = String.sub s pos len in
           add_to_term s)
        flush_term
    in
    Format.pp_set_max_boxes t 42 ;
    Format.pp_set_ellipsis_text t "..." ;
    Format.pp_set_margin t 60 ;
    Format.pp_set_max_indent t 30 ;
    t
end

module Make (L : Language) = struct

  (** Parse the contents from a file, using a given [parser]. *)
  let read_file parser (fn, str) =
    let lex = Lexing.from_string (str ^"\n") in
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
    let terms = Input.wrap (parser fn) lex in
    terms
          
  (** Load directives from the given file. *)
  let use_file ctx (filename, content) =
    match L.file_parser with
    | Some f ->
      let cmds = read_file f (filename, content) in
      L.exec
        (fun _ _ -> Report.fail "Cannot load files in the web toplevel")
        ctx cmds
    | None ->
      Report.fail "Cannot load files, only interactive shell is available"


  let () =
    Report.report_printer :=
      {Report.batch_mode_printer with
       out = DomRepl.term;
       err = DomRepl.term;
      }

  let eval (name, s) =
    let name = Js_of_ocaml.Js.to_string name in
    let s = Js_of_ocaml.Js.to_string s in
    begin try
        DomRepl.clear_term ();
        let _ = use_file L.initial_environment (name, s) in
        ()
      with err -> Report.report_exception DomRepl.term err
    end ;
    ()

  let register_examples l =
    let open Js_of_ocaml_tyxml.Tyxml_js in
    let elem s =
      Html.(li [a ~a:[a_class ["file"]; a_href ("#"^s); a_title s;
                  a_onclick (fun _ -> DomRepl.load_file s; false);]
              [txt s]])
    in
    let l = Html.ul (List.map elem l) in
    Register.id ~keep:true "examples" [l]
  
  let main ~(default: string) ~(files : string list) () =
    register_examples files;
    DomRepl.set_lang_name L.name;
    Js_of_ocaml.Js.export "REPL" (object%js
      method eval name s = eval (name, s)
      val default = Js_of_ocaml.Js.string default
      val examples =
        Js_of_ocaml.Js.array @@ Array.of_list @@
        List.map Js_of_ocaml.Js.string files
    end)
    
end
