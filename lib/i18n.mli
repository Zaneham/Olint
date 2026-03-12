(* i18n.mli — public interface for message localisation *)

val msg  : string -> string
val hint : string -> string option
val load : string -> unit
