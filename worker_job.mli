(*
   Run a single job remotely or locally.
*)

type json = string
type job_status = Api_t.job_status

val register_handler :
  string -> (Worker_jobid.t -> json -> job_status Lwt.t) -> unit
  (* Usage: `register_job_handler job_type handler`.
     The handler interprets the json data of the job specification
     and returns whether the job with this ID should be removed
     from the table after execution (see `run_all)`.
  *)

val run : Worker_t.job -> job_status Lwt.t
  (* Run a job, locally. *)
