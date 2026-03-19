(* bool_redundancy.ml — W008
   `if x then true else false` is just `x`.
   `if x then false else true` is `not x`.
   The sort of thing that reveals someone
   learned to program in Java last week. *)

open Parsetree
open Ast_iterator

let is_bool_lit e v =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Longident.Lident s; _ }, None) -> s = v
  | _ -> false

let chk src ast =
  let diags = ref [] in
  let it = { default_iterator with
    expr = (fun self e ->
      (match e.pexp_desc with
       | Pexp_ifthenelse (cond, then_e, Some else_e) ->
         if is_bool_lit then_e "true" && is_bool_lit else_e "false" then
           let ct = Diagnostic.span src cond.pexp_loc in
           diags := { Diagnostic.
             rid  = "W008";
             sev  = Warn;
             msg  = I18n.msg "W008";
             loc  = e.pexp_loc;
             hint = I18n.hint "W008";
             fix  = [{ fix_loc = e.pexp_loc; fix_txt = ct }];
           } :: !diags
         else if is_bool_lit then_e "false" && is_bool_lit else_e "true" then
           let ct = Diagnostic.span src cond.pexp_loc in
           diags := { Diagnostic.
             rid  = "W008";
             sev  = Warn;
             msg  = I18n.msg "W008.neg";
             loc  = e.pexp_loc;
             hint = I18n.hint "W008";
             fix  = [{ fix_loc = e.pexp_loc;
                        fix_txt = "not (" ^ ct ^ ")" }];
           } :: !diags
       | _ -> ());
      default_iterator.expr self e);
  } in
  it.structure it ast;
  List.rev !diags

let rule : Rule.t = {
  id   = "W008";
  name = "bool-redundancy";
  desc = "if x then true else false — just use x";
  sev  = Warn;
  chk  = chk;
}
