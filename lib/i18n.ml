(* i18n.ml — message localisation.
   Stolen wholesale from BarraCUDA's bc_err.c,
   minus the 32KB static buffer because OCaml
   handles memory like a responsible adult.

   Every diagnostic has a stable ID. English compiled in.
   External files loaded via --lang override messages.
   A developer anywhere can search "olint W003"
   regardless of what language the output is in. *)

(* compiled-in english defaults — the source of truth *)
let dflt = Hashtbl.create 32

let () =
  List.iter (fun (k, v) -> Hashtbl.replace dflt k v) [
    ("W001",      "unused open: `open %s` is not referenced");
    ("W001.hint", "remove the open or use a qualified name");
    ("W002",      "redundant match: every arm returns its pattern unchanged");
    ("W002.hint", "replace the entire match with the scrutinee");
    ("W003",      "List.length in comparison is O(n); compare against [] instead");
    ("W003.hint", "use `xs <> []` or `xs = []` for emptiness checks");
    ("W004",      "List.nth is O(n) and partial; use pattern matching instead");
    ("W004.hint", "if you need indexing, consider Array.t");
    ("W005",      "%s.%s may raise; consider a safe alternative");
    ("W005.hint", "use %s.%s_opt or pattern matching");
    ("W006",      "naming: `%s` should be snake_case");
    ("W006.mod",  "naming: module `%s` should be PascalCase");
    ("W006.typ",  "naming: type `%s` should be snake_case");
    ("W008",      "`if x then true else false` is redundant; use `x` directly");
    ("W008.neg",  "`if x then false else true` is redundant; use `not x`");
    ("W008.hint", "remove the if/else and use the condition directly");
    ("W009",      "`fun x -> f x` can be simplified to `f`");
    ("W009.hint", "remove the wrapper function, pass f directly");
    ("W010",      "%s.%s inside try block; consider the _opt variant");
    ("W010.hint", "use %s.%s%s instead of catching the exception");
    ("E000",      "syntax error: %s");
    ("E000.int",  "internal error: %s");
    ("E000.hint", "fix the syntax error first");
  ]

(* translation overlay — loaded from external file *)
let xlat : (string, string) Hashtbl.t = Hashtbl.create 16

let msg key =
  match Hashtbl.find_opt xlat key with
  | Some s -> s
  | None ->
    match Hashtbl.find_opt dflt key with
    | Some s -> s
    | None -> key

let hint key =
  let hk = key ^ ".hint" in
  match Hashtbl.find_opt xlat hk with
  | Some s -> Some s
  | None -> Hashtbl.find_opt dflt hk

(* load a translation file.
   format: "W001=message text" per line.
   # comments. blank lines ignored.
   bounded: max 512 lines because paranoia is a virtue. *)
let load path =
  let ic = open_in path in
  let n = ref 0 in
  (try while !n < 512 do
    let line = input_line ic in
    incr n;
    let len = String.length line in
    if len = 0 || line.[0] = '#' then ()
    else
      match String.index_opt line '=' with
      | None -> ()
      | Some i when i < 1 -> ()
      | Some i ->
        let key = String.sub line 0 i in
        let v = String.sub line (i + 1) (len - i - 1) in
        Hashtbl.replace xlat key v
  done with End_of_file -> ());
  close_in ic
