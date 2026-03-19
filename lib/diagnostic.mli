(* diagnostic.mli — public interface for diagnostic type *)

type sev = Err | Warn | Info

type fix = { fix_loc : Location.t; fix_txt : string }

type t = {
  rid  : string;
  sev  : sev;
  msg  : string;
  loc  : Location.t;
  hint : string option;
  fix  : fix list;
}

val span        : string -> Location.t -> string
val sev_rank    : sev -> int
val sev_str     : sev -> string
val cmp         : t -> t -> int
val pp          : t -> string
val to_json     : t -> string
val to_json_all : t list -> string
