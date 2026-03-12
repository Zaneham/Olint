(* unused_open.ml — W001
   Catches `open Foo` where Foo is visibly unused.
   Parsetree-only: we can't see unqualified names resolved
   through the open, so this is conservative. Still catches
   the obvious corpses. *)

open Parsetree
open Ast_iterator

(* extract root module name from a Longident.t *)
let rec root_mod = function
  | Longident.Lident s -> Some s
  | Longident.Ldot (m, _) -> root_mod m.Location.txt
  | Longident.Lapply (m, _) -> root_mod m.Location.txt

(* harvest every module name used via qualified access *)
let grab ast =
  let used = Hashtbl.create 64 in
  let it = { default_iterator with
    expr = (fun self e ->
      (match e.pexp_desc with
       | Pexp_ident { txt = Longident.Ldot (m, _); _ } ->
         (match root_mod m.Location.txt with
          | Some s -> Hashtbl.replace used s true
          | None -> ())
       | _ -> ());
      default_iterator.expr self e);
    typ = (fun self t ->
      (match t.ptyp_desc with
       | Ptyp_constr ({ txt = Longident.Ldot (m, _); _ }, _) ->
         (match root_mod m.Location.txt with
          | Some s -> Hashtbl.replace used s true
          | None -> ())
       | _ -> ());
      default_iterator.typ self t);
  } in
  it.structure it ast;
  used

(* extract the simple name from a Longident *)
let lid_name = function
  | Longident.Lident s -> s
  | Longident.Ldot (_, s) -> s.Location.txt
  | Longident.Lapply (_, _) -> ""

(* find opens, cross-reference with usage *)
let chk ast =
  let used = grab ast in
  let diags = ref [] in
  List.iter (fun si ->
    match si.pstr_desc with
    | Pstr_open { popen_expr = { pmod_desc = Pmod_ident { txt = lid; loc }; _ }; _ } ->
      let name = lid_name lid in
      if name <> "" && not (Hashtbl.mem used name) then
        diags := { Diagnostic.
          rid  = "W001";
          sev  = Warn;
          msg  = Printf.sprintf (Scanf.format_from_string (I18n.msg "W001") "%s") name;
          loc  = loc;
          hint = I18n.hint "W001";
        } :: !diags
    | _ -> ()
  ) ast;
  List.rev !diags

let rule : Rule.t = {
  id   = "W001";
  name = "unused-open";
  desc = "Flags open statements where the module appears unused";
  sev  = Warn;
  chk  = chk;
}
