(* diagnostic.ml — the bad news, neatly packaged.
   Because if your code has problems, the least
   we can do is tell you about them politely. *)

type sev =
  | Err
  | Warn
  | Info

type t = {
  rid  : string;
  sev  : sev;
  msg  : string;
  loc  : Location.t;
  hint : string option;
}

let sev_rank = function
  | Err  -> 2
  | Warn -> 1
  | Info -> 0

let sev_str = function
  | Err  -> "E"
  | Warn -> "W"
  | Info -> "I"

let cmp a b =
  let c = String.compare a.loc.loc_start.pos_fname b.loc.loc_start.pos_fname in
  if c <> 0 then c
  else
    let c = compare a.loc.loc_start.pos_lnum b.loc.loc_start.pos_lnum in
    if c <> 0 then c
    else compare a.loc.loc_start.pos_cnum b.loc.loc_start.pos_cnum

(* file:line:col [W001] message *)
let pp d =
  let p = d.loc.loc_start in
  let col = p.pos_cnum - p.pos_bol + 1 in
  let base =
    Printf.sprintf "%s:%d:%d [%s] %s"
      p.pos_fname p.pos_lnum col
      d.rid d.msg
  in
  match d.hint with
  | None   -> base
  | Some h -> Printf.sprintf "%s\n  hint: %s" base h

(* escape a string for JSON — the bare minimum *)
let esc s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | '\t' -> Buffer.add_string b "\\t"
    | c    -> Buffer.add_char b c
  ) s;
  Buffer.contents b

(* JSON object for one diagnostic *)
let to_json d =
  let p = d.loc.loc_start in
  let col = p.pos_cnum - p.pos_bol + 1 in
  let hint_f = match d.hint with
    | None   -> ""
    | Some h -> Printf.sprintf ",\"hint\":\"%s\"" (esc h)
  in
  Printf.sprintf
    "{\"file\":\"%s\",\"line\":%d,\"col\":%d,\"rule\":\"%s\",\"severity\":\"%s\",\"message\":\"%s\"%s}"
    (esc p.pos_fname) p.pos_lnum col
    d.rid (sev_str d.sev) (esc d.msg) hint_f

(* JSON array of diagnostics *)
let to_json_all ds =
  let items = List.map to_json ds in
  "[" ^ String.concat "," items ^ "]"
