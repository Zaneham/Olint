(* suppress.ml — inline suppression via comments.
   Because the difference between a useful tool and
   an annoying one is the ability to say "yes, I know,
   shut up about it."

   Recognises:
     olint:disable W001        — suppress to end of file
     olint:disable W001 W003   — suppress multiple
     olint:enable W001         — re-enable
     olint:disable-next-line W001 — suppress next line only *)

type dir =
  | Dis  of string list * int
  | Ena  of string list * int
  | Next of string list * int

(* parse suppression directives from raw source text *)
let scan src =
  let dirs = ref [] in
  let lines = String.split_on_char '\n' src in
  List.iteri (fun i line ->
    let ln = i + 1 in
    let line = String.trim line in
    (* hunt for comment-open sequences in the line *)
    let rec find pos =
      match String.index_from_opt line pos '(' with
      | None -> ()
      | Some p when p + 2 < String.length line && line.[p+1] = '*' ->
        (* find the closing star-paren *)
        let rec close q =
          if q + 1 >= String.length line then None
          else if line.[q] = '*' && line.[q+1] = ')' then Some q
          else close (q + 1)
        in
        (match close (p + 2) with
         | Some q ->
           let body = String.trim (String.sub line (p + 2) (q - p - 2)) in
           let parts = String.split_on_char ' ' body in
           (match parts with
            | "olint:disable" :: ids when ids <> [] ->
              dirs := Dis (ids, ln) :: !dirs
            | "olint:enable" :: ids when ids <> [] ->
              dirs := Ena (ids, ln) :: !dirs
            | "olint:disable-next-line" :: ids when ids <> [] ->
              dirs := Next (ids, ln) :: !dirs
            | _ -> ());
           find (q + 2)
         | None -> find (p + 1))
      | Some p -> find (p + 1)
    in
    find 0
  ) lines;
  List.rev !dirs

(* build suppressed (rule_id, line) pairs *)
let build dirs max_line =
  let sup = Hashtbl.create 16 in
  let active : (string, int) Hashtbl.t = Hashtbl.create 8 in
  let dirs = List.sort (fun a b ->
    let la = match a with Dis (_, l) | Ena (_, l) | Next (_, l) -> l in
    let lb = match b with Dis (_, l) | Ena (_, l) | Next (_, l) -> l in
    compare la lb
  ) dirs in
  List.iter (fun d ->
    match d with
    | Dis (ids, ln) ->
      List.iter (fun id -> Hashtbl.replace active id ln) ids
    | Ena (ids, ln) ->
      List.iter (fun id ->
        (match Hashtbl.find_opt active id with
         | Some from ->
           for l = from to ln do
             Hashtbl.replace sup (id, l) true
           done;
           Hashtbl.remove active id
         | None -> ())
      ) ids
    | Next (ids, ln) ->
      List.iter (fun id ->
        Hashtbl.replace sup (id, ln + 1) true
      ) ids
  ) dirs;
  (* flush still-active disables to end of file *)
  Hashtbl.iter (fun id from ->
    for l = from to max_line do
      Hashtbl.replace sup (id, l) true
    done
  ) active;
  sup

(* filter diagnostics, removing suppressed ones *)
let filter src (ds : Diagnostic.t list) =
  let dirs = scan src in
  if dirs = [] then ds
  else
    let max_ln = List.fold_left (fun mx (d : Diagnostic.t) ->
      max mx d.loc.loc_start.pos_lnum
    ) 0 ds in
    let sup = build dirs (max_ln + 1) in
    List.filter (fun (d : Diagnostic.t) ->
      not (Hashtbl.mem sup (d.rid, d.loc.loc_start.pos_lnum))
    ) ds
