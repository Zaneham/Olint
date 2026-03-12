(* test fixture: naming conventions.
   A gallery of naming sins from someone who
   clearly spent too long writing Java. *)

(* W006: camelCase value — the continental influence *)
let myFunction x = x + 1

(* W006: camelCase type — unspeakable *)
type myType = Foo | Bar

(* these are fine and should be silent *)
let good_name x = x + 1
type good_type = A | B
module GoodModule = struct let x = 1 end
