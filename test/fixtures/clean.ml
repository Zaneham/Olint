(* test fixture: clean code.
   Proof that it is possible to write OCaml
   without upsetting anyone. Should produce
   zero diagnostics. *)

let add a b = a + b

let greet name =
  Printf.sprintf "hello, %s" name

type colour = Red | Green | Blue

let describe = function
  | Red   -> "red"
  | Green -> "green"
  | Blue  -> "blue"

module Utils = struct
  let id x = x
  let flip f x y = f y x
end
