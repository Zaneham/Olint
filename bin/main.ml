(* main.ml — CLI entry point.
   The bit that turns a library into a tool.
   Uses cmdliner because life is too short for
   hand-rolled argument parsing. *)

type fmt = Plain | Json

let find_ml dir =
  let rec walk acc path =
    if Sys.is_directory path then
      let cs = Sys.readdir path in
      Array.fold_left (fun acc c ->
        walk acc (Filename.concat path c)
      ) acc cs
    else if Filename.check_suffix path ".ml" then
      path :: acc
    else
      acc
  in
  List.rev (walk [] dir)

let resolve paths =
  List.concat_map (fun p ->
    if Sys.is_directory p then find_ml p
    else if Filename.check_suffix p ".ml" then [p]
    else []
  ) paths

(* --list-rules: self-documenting rule catalogue *)
let list_rules () =
  Printf.printf "%-6s %-22s %s  %s\n" "ID" "Name" "Sev" "Description";
  Printf.printf "%s\n" (String.make 72 '-');
  List.iter (fun (r : Olint_lib.Rule.t) ->
    Printf.printf "%-6s %-22s [%s]  %s\n"
      r.id r.name
      (Olint_lib.Diagnostic.sev_str r.sev)
      r.desc
  ) Olint_lib.Olint.rules;
  0

let run paths min_sev lang fmt do_list =
  if do_list then list_rules ()
  else begin
    (match lang with
     | Some p -> Olint_lib.Olint.load_lang p
     | None -> ());
    let files = resolve paths in
    if files = [] then (
      Printf.eprintf "olint: no .ml files found\n";
      2
    ) else
      let ds = Olint_lib.Olint.lint_all files in
      let ds = List.filter (fun d ->
        Olint_lib.Diagnostic.sev_rank d.Olint_lib.Diagnostic.sev >= min_sev
      ) ds in
      if ds = [] then 0
      else (
        (match fmt with
         | Plain ->
           List.iter (fun d ->
             Printf.printf "%s\n" (Olint_lib.Diagnostic.pp d)
           ) ds
         | Json ->
           Printf.printf "%s\n" (Olint_lib.Diagnostic.to_json_all ds));
        let has_err = List.exists (fun d ->
          d.Olint_lib.Diagnostic.sev = Olint_lib.Diagnostic.Err
        ) ds in
        if has_err then 2 else 1
      )
  end

open Cmdliner

let paths =
  Arg.(value & pos_all string [] &
       info [] ~docv:"PATH" ~doc:"Files or directories to lint")

let sev =
  let doc = "Minimum severity: error, warn, or info" in
  let svs = [
    ("error", Olint_lib.Diagnostic.sev_rank Olint_lib.Diagnostic.Err);
    ("warn",  Olint_lib.Diagnostic.sev_rank Olint_lib.Diagnostic.Warn);
    ("info",  Olint_lib.Diagnostic.sev_rank Olint_lib.Diagnostic.Info);
  ] in
  Arg.(value & opt (enum svs) (Olint_lib.Diagnostic.sev_rank Olint_lib.Diagnostic.Info) &
       info ["s"; "severity"] ~docv:"LEVEL" ~doc)

let lang =
  let doc = "Path to translation file (e.g. lang/en.txt)" in
  Arg.(value & opt (some string) None &
       info ["lang"] ~docv:"FILE" ~doc)

let fmt =
  let doc = "Output format: plain or json" in
  let fmts = [("plain", Plain); ("json", Json)] in
  Arg.(value & opt (enum fmts) Plain &
       info ["f"; "format"] ~docv:"FMT" ~doc)

let do_list =
  let doc = "List all registered rules and exit" in
  Arg.(value & flag & info ["list-rules"] ~doc)

let cmd =
  let doc = "a linter for OCaml" in
  let info = Cmd.info "olint" ~version:"0.1.0" ~doc
    ~man:[
      `S Manpage.s_description;
      `P "olint analyses OCaml source files for common mistakes, \
          antipatterns, and style violations. Think clippy for OCaml.";
      `S "INLINE SUPPRESSION";
      `P "Suppress specific rules with comments:";
      `Pre "  (* olint:disable W001 *)        \
            — suppress to end of file\n  \
            (* olint:disable-next-line W001 *) \
            — suppress next line only\n  \
            (* olint:enable W001 *)            \
            — re-enable";
      `S "LOCALISATION";
      `P "Use --lang to load a translation file. Format: \
          'W001=message text', one per line.";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/user/olint/issues";
    ]
  in
  Cmd.v info Term.(const run $ paths $ sev $ lang $ fmt $ do_list)

let () = exit (Cmd.eval' cmd)
