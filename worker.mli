(*
   System for scheduling jobs at specific times in the future.

   It relies on a mysql table and a cron job that runs every minute
   and executes the jobs whose time has come.
*)

val get_job : Worker_t.jobid -> Worker_t.job option Lwt.t
val remove_job : Worker_t.jobid -> unit Lwt.t

type json = string

(* TODO: provide a way to not overwrite a given job unless it is
   an intentional reschedule *)
val schedule_job :
  ?expiry:Worker_t.timestamp ->
  ?do_not_retry:bool ->
  Worker_t.jobid ->
  Worker_t.timestamp (* when the job should run *) ->
  string (* name of the handler that should handle the job data *) ->
  json (* job data (JSON) *) ->
  Worker_t.job Lwt.t
  (* Schedule or reschedule a job.
     Job IDs are created with the Worker_jobid module. *)

val run_all :
  (Worker_jobid.t -> string -> json -> bool Lwt.t) -> unit Lwt.t
  (* Run all the jobs whose time has come.

     The handler is called as [handler jobid action_type action_details]
     and returns a boolean that indicates whether the job should
     be removed from the table upon success. Returning false
     allows the job to rescheduled at some future date by the handler itself.

     If the handler raises an exception, the job is removed from the table
     unless it is scheduled for a retry.
  *)
