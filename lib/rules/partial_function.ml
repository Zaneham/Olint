(* partial_function.ml — W005
   List.hd on monday: fine.
   List.hd in production at 3am: less fine.
   We flag the obvious partial functions used without
   any visible exception handling. *)

open Parsetree
open Ast_iterator

(* the usual suspects *)
let partials = [
  ("List", "hd");
  ("List", "tl");
  ("List", "find");
  ("List", "assoc");
  ("Array", "get");
  ("Queue", "pop");
  ("Queue", "peek");
  ("Stack", "pop");
  ("Stack", "top");
]

let is_partial m f =
  List.exists (fun (m', f') -> m = m' && f = f') partials

(* check a Longident for partial function calls *)
let chk_lid lid =
  match lid with
  | Longident.Ldot (ml, fl) ->
    let m = match ml.Location.txt with
      | Longident.Lident s -> s
      | _ -> ""
    in
    let f = fl.Location.txt in
    if is_partial m f then Some (m, f) else None
  | _ -> None

let chk _src ast =
  let diags = ref [] in
  let it = { default_iterator with
    expr = (fun self e ->
      (match e.pexp_desc with
       | Pexp_ident { txt = lid; loc } ->
         (match chk_lid lid with
          | Some (m, f) ->
            diags := { Diagnostic.
              rid  = "W005";
              sev  = Warn;
              msg  = Printf.sprintf (Scanf.format_from_string (I18n.msg "W005") "%s%s") m f;
              loc  = loc;
              hint = (match I18n.hint "W005" with
                      | Some fmt ->
                        Some (Printf.sprintf
                          (Scanf.format_from_string fmt "%s%s") m f)
                      | None -> None);
              fix  = [];
            } :: !diags
          | None -> ())
       | _ -> ());
      default_iterator.expr self e);
  } in
  it.structure it ast;
  List.rev !diags

let rule : Rule.t = {
  id   = "W005";
  name = "partial-function";
  desc = "Flags partial functions used without visible guards";
  sev  = Warn;
  chk  = chk;
}
