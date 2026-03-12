(* eta_reduce.ml — W009
   `fun x -> f x` is just `f`.
   The extra lambda is the functional programming
   equivalent of saying "I'd like to introduce you
   to someone you've already met." *)

open Parsetree
open Ast_iterator

(* detect `fun x -> f x` where x is a simple variable *)
let is_eta e =
  match e.pexp_desc with
  | Pexp_function (params, _, Pfunction_body body) ->
    (match params with
     | [{ pparam_desc = Pparam_val (Asttypes.Nolabel, None,
            { ppat_desc = Ppat_var { txt = v; _ }; _ }); _ }] ->
       (* body must be `f x` where x matches the parameter *)
       (match body.pexp_desc with
        | Pexp_apply (_, [(Asttypes.Nolabel,
            { pexp_desc = Pexp_ident { txt = Longident.Lident v'; _ }; _ })]) ->
          String.equal v v'
        | _ -> false)
     | _ -> false)
  | _ -> false

let chk ast =
  let diags = ref [] in
  let it = { default_iterator with
    expr = (fun self e ->
      (if is_eta e then
         diags := { Diagnostic.
           rid  = "W009";
           sev  = Info;
           msg  = I18n.msg "W009";
           loc  = e.pexp_loc;
           hint = I18n.hint "W009";
         } :: !diags);
      default_iterator.expr self e);
  } in
  it.structure it ast;
  List.rev !diags

let rule : Rule.t = {
  id   = "W009";
  name = "eta-reduce";
  desc = "fun x -> f x can be simplified to f";
  sev  = Info;
  chk  = chk;
}
