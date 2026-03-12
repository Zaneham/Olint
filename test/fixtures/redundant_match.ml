(* test fixture: redundant matches.
   Every arm faithfully returns exactly what it was given.
   The very definition of doing nothing, with great ceremony. *)

type colour = Red | Green | Blue

(* this is just `x`, wearing a disguise *)
let f x = match x with
  | Red -> Red
  | Green -> Green
  | Blue -> Blue

(* boolean identity — truly the null hypothesis *)
let g x = match x with
  | true -> true
  | false -> false
