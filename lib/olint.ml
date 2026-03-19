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
    let ds = List.concat_map (fun r -> r.Rule.chk src ast) rules in
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
       fix  = [];
     }]
  | e ->
    [{ Diagnostic.
       rid  = "E000";
       sev  = Err;
       msg  = Printf.sprintf (Scanf.format_from_string (I18n.msg "E000.int") "%s")
                (Printexc.to_string e);
       loc  = Location.{ none with loc_start = { none.loc_start with pos_fname = path } };
       hint = None;
       fix  = [];
     }]

(* batch lint, sorted across all files *)
let lint_all paths =
  let ds = List.concat_map lint paths in
  List.sort Diagnostic.cmp ds

(* apply text replacements bottom-to-top by byte offset.
   Working backwards preserves earlier offsets — the one
   trick compilers and text editors agree on. *)
let patch src fixes =
  let fs = List.sort (fun a b ->
    compare
      b.Diagnostic.fix_loc.loc_start.pos_cnum
      a.Diagnostic.fix_loc.loc_start.pos_cnum
  ) fixes in
  let buf = Buffer.create (String.length src) in
  Buffer.add_string buf src;
  List.iter (fun f ->
    let s = f.Diagnostic.fix_loc.loc_start.pos_cnum in
    let e = f.Diagnostic.fix_loc.loc_end.pos_cnum in
    let cur = Buffer.contents buf in
    Buffer.clear buf;
    Buffer.add_string buf (String.sub cur 0 s);
    Buffer.add_string buf f.Diagnostic.fix_txt;
    Buffer.add_string buf (String.sub cur e (String.length cur - e))
  ) fs;
  Buffer.contents buf

(* lint a file and apply all fixes in-place. Returns fix count. *)
let apply_file path =
  let ds = lint path in
  let fixes = List.concat_map (fun (d : Diagnostic.t) -> d.fix) ds in
  if fixes = [] then 0
  else begin
    let src = slurp path in
    let out = patch src fixes in
    let oc = open_out_bin path in
    output_string oc out;
    close_out oc;
    List.length fixes
  end

let load_lang = I18n.load
