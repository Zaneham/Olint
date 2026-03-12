(* rule.ml — a lint rule is just a function with opinions.
   We keep the interface minimal because every abstraction
   is a potential regret. *)

type chk = Parsetree.structure -> Diagnostic.t list

type t = {
  id   : string;     (* "W001" *)
  name : string;     (* "unused-open" *)
  desc : string;     (* human-readable explanation *)
  sev  : Diagnostic.sev;
  chk  : chk;        (* the actual analysis *)
}
