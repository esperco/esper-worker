(* table of actions to be executed sometime in the future *)
module Job = Mysql_access_kv.Make (struct
  let tblname = "job"
  module Key = Jobid
  module Value = struct
    type t = Worker_t.job
    let of_string = Worker_j.job_of_string
    let to_string x = Worker_j.string_of_job x
  end
  module Ord = Util_time
  let create_ord k v = v.Worker_t.start
  let update_ord = Some (fun k v _ -> v.Worker_t.start)
end)
