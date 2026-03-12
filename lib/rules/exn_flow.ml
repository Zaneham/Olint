(* exn_flow.ml — W010
   Using exceptions for control flow when _opt
   alternatives exist. The try/with Not_found
   around Hashtbl.find is the OCaml equivalent
   of goto — it works, but we can do better. *)

open Parsetree
open Ast_iterator

(* functions that have _opt alternatives *)
let has_opt = [
  ("List",    "find");
  ("List",    "assoc");
  ("Hashtbl", "find");
  ("Map",     "find");
  ("Sys",     "getenv");
]

let chk_lid lid =
  match lid with
  | Longident.Ldot (ml, fl) ->
    let m = match ml.Location.txt with
      | Longident.Lident s -> s
      | _ -> ""
    in
    let f = fl.Location.txt in
    if List.exists (fun (m', f') -> m = m' && f = f') has_opt
    then Some (m, f) else None
  | _ -> None

(* is there a call to a function with _opt inside a try block? *)
let chk ast =
  let diags = ref [] in
  let it = { default_iterator with
    expr = (fun self e ->
      (match e.pexp_desc with
       | Pexp_try (body, _cases) ->
         (* scan body for calls to functions with _opt variants *)
         let inner = { default_iterator with
           expr = (fun self2 ie ->
             (match ie.pexp_desc with
              | Pexp_apply (
                  { pexp_desc = Pexp_ident { txt = lid; _ }; pexp_loc; _ },
                  _) ->
                (match chk_lid lid with
                 | Some (m, f) ->
                   diags := { Diagnostic.
                     rid  = "W010";
                     sev  = Info;
                     msg  = Printf.sprintf
                       (Scanf.format_from_string (I18n.msg "W010") "%s%s")
                       m f;
                     loc  = pexp_loc;
                     hint = (match I18n.hint "W010" with
                       | Some fmt ->
                         Some (Printf.sprintf
                           (Scanf.format_from_string fmt "%s%s%s")
                           m f "_opt")
                       | None -> None);
                   } :: !diags
                 | None -> ())
              | _ -> ());
             default_iterator.expr self2 ie)
         } in
         inner.expr inner body
       | _ -> ());
      default_iterator.expr self e);
  } in
  it.structure it ast;
  List.rev !diags

let rule : Rule.t = {
  id   = "W010";
  name = "exn-control-flow";
  desc = "Exception-heavy control flow where _opt exists";
  sev  = Info;
  chk  = chk;
}
