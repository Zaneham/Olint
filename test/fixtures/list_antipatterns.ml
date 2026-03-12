(* test fixture: list antipatterns.
   The kind of code that works until the list has
   a million elements, at which point you have
   bigger problems than linting. *)

(* W003: O(n) length check *)
let is_empty xs = List.length xs = 0
let has_items xs = List.length xs > 0

(* W004: indexing into a linked list, bold move *)
let third xs = List.nth xs 2
