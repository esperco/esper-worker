(*
   System for scheduling jobs at specific times in the future.

   It relies on a mysql table and a cron job that runs every minute
   and executes the jobs whose time has come.
*)

(* Those methods are not meant to be called directly from outside. *)
class type scheduler =
  object
    method is_real : bool
    method now : unit -> Util_time.t
    method get_job : Worker_jobid.t -> Worker_t.job option Lwt.t
    method job_exists : Worker_jobid.t -> bool Lwt.t
    method add_job : Worker_t.job -> unit Lwt.t
    method remove_job : Worker_jobid.t -> unit Lwt.t
    method update_job :
      'a. Worker_t.jobid ->
          (Worker_t.job option ->
          (Worker_t.job option * 'a) Lwt.t) ->
          'a Lwt.t

    method run_due_jobs : (Worker_t.job -> unit Lwt.t) -> unit Lwt.t
  end

val real_scheduler : scheduler

val scheduler : scheduler ref
  (* The current scheduler in use. It is initially set to `real_scheduler`
     which uses a MySQL table and real time.

     It can be replaced by an alternate scheduler for testing purposes.
  *)

val get_job : Worker_jobid.t -> Worker_t.job option Lwt.t
val add_job : Worker_t.job -> unit Lwt.t
val remove_job : Worker_jobid.t -> unit Lwt.t

type json = string

val default_max_attempts : int
  (* Default value of the `max_attempts` parameter *)

val schedule_job :
  ?ignore_if_exists:bool ->
  ?expiry:Worker_t.timestamp ->
  ?max_attempts:int ->
  Worker_jobid.t ->
  Worker_t.timestamp (* when the job should run *) ->
  string (* name of the handler that should handle the job data *) ->
  json (* job data (JSON) *) ->
  Worker_t.job Lwt.t
  (* Schedule a job.
     Fail if a job with this ID is already scheduled,
     unless `ignore_if_exists` is true.
     Job IDs are created with the Worker_jobid module.
  *)

val reschedule_job :
  ?expiry:Worker_t.timestamp ->
  ?max_attempts:int ->
  Worker_jobid.t ->
  Worker_t.timestamp ->
  string ->
  json ->
  Worker_t.job Lwt.t
  (* Same as `schedule_job` but doesn't fail if a job is already
     scheduled with this ID, in which case the previous job is rescheduled.
     This is not the same as an automatic retry due to an uncaught
     exception. The number of attempts is reset to 0. *)

val register_job_handler :
  string -> (Worker_jobid.t -> json -> bool Lwt.t) -> unit
  (* Usage: `register_job_handler job_type handler`.
     The handler interprets the json data of the job specification
     and returns whether the job with this ID should be removed
     from the table after execution (see `run_all)`.
  *)

val run_due_jobs : unit -> unit Lwt.t
  (* Run all the jobs whose time has come.

     The handler is called as [handler jobid action_type action_details]
     and returns a boolean that indicates whether the job should
     be removed from the table upon success. Returning false
     allows the job to rescheduled at some future date by the handler itself.

     If the handler raises an exception, the job is removed from the table
     unless it is scheduled for a retry.
  *)
