(* olint.mli — public interface for the linter engine *)

val rules      : Rule.t list
val parse      : string -> Parsetree.structure
val lint       : string -> Diagnostic.t list
val lint_all   : string list -> Diagnostic.t list
val patch      : string -> Diagnostic.fix list -> string
val apply_file : string -> int
val load_lang  : string -> unit
