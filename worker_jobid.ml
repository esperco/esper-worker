(* Job ID *)

type t = {
  namespace: string;
  id: string;
}

module Random_id = Id16.Make (
  struct
    let short_id_name = "jobid"
    let long_id_name = "job ID"
    let module_name = "Worker_jobid"
  end
)

let namespace_re = Pcre.regexp "\\A[A-Za-z0-9_-]+\\Z"

let is_valid_namespace s =
  Pcre.pmatch ~rex:namespace_re s

let check_namespace s =
  if not (is_valid_namespace s) then
    invalid_arg ("Worker_jobid.make: invalid namespace " ^ s)

let make ~namespace ?(id = Random_id.(to_string (make ()))) () =
  check_namespace namespace;
  { namespace; id }

let of_string s =
  let namespace, id =
    try BatString.split s ~by:":"
    with Not_found -> "default", s
  in
  check_namespace namespace;
  { namespace; id }

let to_string x =
  x.namespace ^ ":" ^ x.id

let wrap   = of_string
let unwrap = to_string
