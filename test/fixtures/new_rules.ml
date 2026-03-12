(* test fixture: boolean redundancy and friends.
   Also tests inline suppression and the new rules. *)

(* W008: the boolean tautology *)
let is_big x = if x > 10 then true else false
let is_small x = if x < 5 then false else true

(* W009: eta-reducible — the unnecessary introduction *)
let mapped xs = List.map (fun x -> succ x) xs

(* W010: exception control flow *)
let safe_find tbl k =
  try Hashtbl.find tbl k
  with Not_found -> ""

(* this one is suppressed — should NOT fire *)
(* olint:disable W008 *)
let also_big x = if x > 100 then true else false
(* olint:enable W008 *)

(* next-line suppression *)
(* olint:disable-next-line W009 *)
let also_mapped xs = List.map (fun x -> succ x) xs
