(* olint.ml — the engine. Parses files, runs rules,
   collects the wreckage. Keeps no state because
   functional programming means never having to say
   you're sorry about shared mutability. *)

(* all registered rules — the complete arsenal *)
let rules : Rule.t list = [
  Unused_open.rule;
  Redundant_match.rule;
] @ List_antipattern.rules @ [
  Partial_function.rule;
  Naming.rule;
  Bool_redundancy.rule;
  Eta_reduce.rule;
  Exn_flow.rule;
]

(* parse a .ml file into Parsetree *)
let parse path =
  let ic = open_in path in
  let lb = Lexing.from_channel ic in
  Location.init lb path;
  let ast =
    try Parse.implementation lb
    with e ->
      close_in ic;
      raise e
  in
  close_in ic;
  ast

(* slurp a file — binary mode to avoid \r\n length mismatch on windows *)
let slurp path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(* lint one file, return sorted diagnostics *)
let lint path =
  try
    let src = slurp path in
    let lb = Lexing.from_string src in
    Location.init lb path;
    let ast = Parse.implementation lb in
    let ds = List.concat_map (fun r -> r.Rule.chk ast) rules in
    let ds = Suppress.filter src ds in
    List.sort Diagnostic.cmp ds
  with
  | Syntaxerr.Error _ as e ->
    let loc = match e with
      | Syntaxerr.Error e -> Syntaxerr.location_of_error e
      | _ -> Location.none
    in
    [{ Diagnostic.
       rid  = "E000";
       sev  = Err;
       msg  = Printf.sprintf (Scanf.format_from_string (I18n.msg "E000") "%s")
                (Printexc.to_string e);
       loc  = { loc with loc_start = { loc.loc_start with pos_fname = path } };
       hint = I18n.hint "E000";
     }]
  | e ->
    [{ Diagnostic.
       rid  = "E000";
       sev  = Err;
       msg  = Printf.sprintf (Scanf.format_from_string (I18n.msg "E000.int") "%s")
                (Printexc.to_string e);
       loc  = Location.{ none with loc_start = { none.loc_start with pos_fname = path } };
       hint = None;
     }]

(* batch lint, sorted across all files *)
let lint_all paths =
  let ds = List.concat_map lint paths in
  List.sort Diagnostic.cmp ds

let load_lang = I18n.load
