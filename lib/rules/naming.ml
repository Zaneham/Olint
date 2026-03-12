(* naming.ml — W006
   OCaml convention: snake_case for values, PascalCase
   for modules. Enforcing this makes codebases readable
   by anyone, not just the author on a good day. *)

open Parsetree
open Ast_iterator

(* snake_case: lowercase, underscores, digits. operators get a pass *)
let is_snake s =
  String.length s = 0 ||
  s.[0] = '_' ||
  s.[0] = '(' ||
  let ok = ref true in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | '0'..'9' | '_' | '\'' -> ()
    | _ -> ok := false
  ) s;
  !ok

(* PascalCase: starts with uppercase *)
let is_pascal s =
  String.length s > 0 &&
  match s.[0] with 'A'..'Z' -> true | _ -> false

let chk ast =
  let diags = ref [] in
  let it = { default_iterator with
    (* value bindings: must be snake_case *)
    value_binding = (fun self vb ->
      (match vb.pvb_pat.ppat_desc with
       | Ppat_var { txt = name; loc } when not (is_snake name) ->
         diags := { Diagnostic.
           rid  = "W006";
           sev  = Info;
           msg  = Printf.sprintf (Scanf.format_from_string (I18n.msg "W006") "%s") name;
           loc  = loc;
           hint = None;
         } :: !diags
       | _ -> ());
      default_iterator.value_binding self vb);
    (* module bindings: must be PascalCase *)
    module_binding = (fun self mb ->
      (match mb.pmb_name.txt with
       | Some name when not (is_pascal name) ->
         diags := { Diagnostic.
           rid  = "W006";
           sev  = Info;
           msg  = Printf.sprintf (Scanf.format_from_string (I18n.msg "W006.mod") "%s") name;
           loc  = mb.pmb_name.loc;
           hint = None;
         } :: !diags
       | _ -> ());
      default_iterator.module_binding self mb);
    (* type declarations: must be snake_case *)
    type_declaration = (fun self td ->
      let name = td.ptype_name.txt in
      if not (is_snake name) then
        diags := { Diagnostic.
          rid  = "W006";
          sev  = Info;
          msg  = Printf.sprintf (Scanf.format_from_string (I18n.msg "W006.typ") "%s") name;
          loc  = td.ptype_name.loc;
          hint = None;
        } :: !diags;
      default_iterator.type_declaration self td);
  } in
  it.structure it ast;
  List.rev !diags

let rule : Rule.t = {
  id   = "W006";
  name = "naming-convention";
  desc = "snake_case for values/types, PascalCase for modules";
  sev  = Info;
  chk  = chk;
}
