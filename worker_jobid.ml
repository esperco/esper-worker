(* Job ID *)

type t = {
  namespace: string;
    (* For compatibility with old job IDs found in production without
       a namespace, this string can be empty.
       Newly-formed job IDs must have a valid namespace. *)
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
    try
      let namespace, id = BatString.split s ~by:":" in
      check_namespace namespace;
      namespace, id
    with Not_found ->
      Log.logf `Warning "Missing namespace in job ID %S." s;
      "", s
  in
  { namespace; id }

let to_string x =
  match x.namespace with
  | "" ->
      let s = x.id in
      Log.logf `Warning "Missing namespace in job ID %S." s;
      s
  | namespace ->
      namespace ^ ":" ^ x.id

let wrap   = of_string
let unwrap = to_string

let test () =
  assert (to_string (of_string "abc") = "abc");
  assert (to_string (of_string "abc:cde") = "abc:cde");
  assert (of_string "abc:cde" = { namespace = "abc"; id = "cde" });
  assert (try ignore (of_string "ab\nc:cd"); false with _ -> true);
  assert (try ignore (of_string ":c"); false with _ -> true);
  true

let tests = [
  "worker job ID", test;
]
