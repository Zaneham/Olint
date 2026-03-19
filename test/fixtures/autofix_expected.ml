(* test fixture: auto-fix candidates.
   W008 and W009 should all be fixable. *)

(* W008: bool tautology — should become just the condition *)
let is_big x = x > 10

(* W008 negated — should become not (condition) *)
let is_small x = not (x < 5)

(* W009: eta-reducible — should become just succ *)
let mapped xs = List.map succ xs
