(* test fixture: partial functions.
   Using List.hd without a guard is like crossing
   the road with your eyes closed — it works until
   the day it very much doesn't. *)

(* W005: bare use of partial functions *)
let first xs = List.hd xs
let rest xs = List.tl xs
let found xs = List.find (fun x -> x > 0) xs
