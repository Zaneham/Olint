(* redundant_match.ml — W002
   Identity matches: when every arm just returns what
   it matched. The whole expression is a roundabout
   way of saying "x". Nature abhors a tautology. *)

open Parsetree
open Ast_iterator

(* check if a pattern and expression are structurally identical.
   conservative: only handles the obvious cases *)
let rec pat_eq_exp p e =
  match p.ppat_desc, e.pexp_desc with
  | Ppat_var { txt = v; _ }, Pexp_ident { txt = Longident.Lident v'; _ } ->
    String.equal v v'
  | Ppat_construct ({ txt = c; _ }, None),
    Pexp_construct ({ txt = c'; _ }, None) ->
    c = c'
  | Ppat_construct ({ txt = c; _ }, Some (_, cp)),
    Pexp_construct ({ txt = c'; _ }, Some ce) ->
    c = c' && pat_eq_exp cp ce
  | Ppat_constant c1, Pexp_constant c2 ->
    c1 = c2
  | _ -> false

(* at least two arms, all identity *)
let is_id_match = function
  | _ :: _ :: _ as cases ->
    List.for_all (fun c ->
      c.pc_guard = None &&
      pat_eq_exp c.pc_lhs c.pc_rhs
    ) cases
  | _ -> false

let chk ast =
  let diags = ref [] in
  let it = { default_iterator with
    expr = (fun self e ->
      (match e.pexp_desc with
       | Pexp_match (_, cases) when is_id_match cases ->
         diags := { Diagnostic.
           rid  = "W002";
           sev  = Warn;
           msg  = I18n.msg "W002";
           loc  = e.pexp_loc;
           hint = I18n.hint "W002";
         } :: !diags
       | _ -> ());
      default_iterator.expr self e);
  } in
  it.structure it ast;
  List.rev !diags

let rule : Rule.t = {
  id   = "W002";
  name = "redundant-match";
  desc = "Identity matches where every arm returns what it matched";
  sev  = Warn;
  chk  = chk;
}
