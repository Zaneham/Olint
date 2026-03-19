(* list_antipattern.ml — W003 + W004
   W003: List.length in comparisons. O(n) where O(1) will do.
         The sort of thing that works fine until it doesn't.
   W004: List.nth is almost always wrong. If you're indexing
         into a list, you wanted an array. *)

open Parsetree
open Ast_iterator

(* is this `List.length <arg>`? *)
let is_len e =
  match e.pexp_desc with
  | Pexp_apply (
      { pexp_desc = Pexp_ident { txt = Longident.Ldot (m, f); _ }; _ },
      _) ->
    (match m.Location.txt with
     | Longident.Lident "List" -> f.Location.txt = "length"
     | _ -> false)
  | _ -> false

(* is this a qualified `List.nth`? *)
let is_list_nth lid =
  match lid with
  | Longident.Ldot (m, f) ->
    (match m.Location.txt with
     | Longident.Lident "List" -> f.Location.txt = "nth"
     | _ -> false)
  | _ -> false

let cmp_ops = ["="; "<>"; ">"; "<"; ">="; "<="]

let is_cmp op = List.mem op cmp_ops

let chk _src ast =
  let diags = ref [] in
  let it = { default_iterator with
    expr = (fun self e ->
      (match e.pexp_desc with
       (* W003: binary comparison with List.length on either side *)
       | Pexp_apply (
           { pexp_desc = Pexp_ident { txt = Longident.Lident op; _ }; _ },
           [(_, lhs); (_, rhs)])
         when is_cmp op && (is_len lhs || is_len rhs) ->
         diags := { Diagnostic.
           rid  = "W003";
           sev  = Warn;
           msg  = I18n.msg "W003";
           loc  = e.pexp_loc;
           hint = I18n.hint "W003";
           fix  = [];
         } :: !diags
       (* W004: List.nth applied to args *)
       | Pexp_apply (
           { pexp_desc = Pexp_ident { txt = lid; _ }; pexp_loc; _ },
           _) when is_list_nth lid ->
         diags := { Diagnostic.
           rid  = "W004";
           sev  = Warn;
           msg  = I18n.msg "W004";
           loc  = pexp_loc;
           hint = I18n.hint "W004";
           fix  = [];
         } :: !diags
       | _ -> ());
      default_iterator.expr self e);
  } in
  it.structure it ast;
  List.rev !diags

let rules : Rule.t list = [
  { id   = "W003";
    name = "list-length-compare";
    desc = "List.length in comparisons is O(n)";
    sev  = Warn;
    chk  = (fun src ast -> List.filter (fun d -> d.Diagnostic.rid = "W003") (chk src ast));
  };
  { id   = "W004";
    name = "list-nth";
    desc = "List.nth is almost always wrong";
    sev  = Warn;
    chk  = (fun src ast -> List.filter (fun d -> d.Diagnostic.rid = "W004") (chk src ast));
  };
]
