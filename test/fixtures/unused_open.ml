(* test fixture: unused opens.
   Some are dead, some aren't. A miniature crime scene. *)

open Printf
(* Printf is never used — W001 should fire *)

open List
(* List is used below via qualified name — W001 should NOT fire *)

let _ = List.length [1; 2; 3]
