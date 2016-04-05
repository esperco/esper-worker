type t = private {
  namespace: string;
  id: string;
}

val make : namespace:string -> ?id:string -> unit -> t
  (* Create a job ID within a given namespace.
     The namespace must match the following pattern: [A-Za-z0-9_-]+
     If not specified, the id is a random string sufficiently
     complex to avoid accidental collisions in practice. *)

(* Serialization *)

val of_string : string -> t
val to_string : t -> string

val wrap : string -> t
val unwrap : t -> string

val test : unit -> bool
val tests : (string * (unit -> bool)) list
