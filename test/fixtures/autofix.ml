(* test fixture: auto-fix candidates.
   W008 and W009 should all be fixable. *)

(* W008: bool tautology — should become just the condition *)
let is_big x = if x > 10 then true else false

(* W008 negated — should become not (condition) *)
let is_small x = if x < 5 then false else true

(* W009: eta-reducible — should become just succ *)
let mapped xs = List.map (fun x -> succ x) xs
