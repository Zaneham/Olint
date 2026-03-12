(* rule.mli — public interface for lint rules *)

type chk = Parsetree.structure -> Diagnostic.t list

type t = {
  id   : string;
  name : string;
  desc : string;
  sev  : Diagnostic.sev;
  chk  : chk;
}
