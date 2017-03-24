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

val time : unit -> float
val now : unit -> Util_time.t
  (* Return the current time, according to the scheduler, which
     may be much later than the time returned by Unix.time
     or Unix.gettimeofday if we're using the fake scheduler
     (Worker_simulator). *)

val get_job : Worker_jobid.t -> Worker_t.job option Lwt.t
val add_job : Worker_t.job -> unit Lwt.t
val remove_job : Worker_jobid.t -> unit Lwt.t

type json = string

val default_max_attempts : int
  (* Default value of the `max_attempts` parameter *)

(*
   Schedule a new job.

   Use `reschedule_job` instead if the new job specification should silently
   override any existing job.

   If there is already a job with this job ID:

   - The default behavior is to treat this as an error and raise an exception.

   - An alternate behavior can be obtained by setting the `ignore_if_exists`
     flag. In this case, the new job specification is ignored and the
     old job found in the table is returned. In this case, the exiting
     job is unaffected and in particular, the number of retries
     and the maximum number of retries (`max_attempts`) are not reset.

   Options:
   `ignore_if_exists`: see above
   `expiry`:
      date after which the job may not start. Possible reasons
      for a delayed start include system downtime
      and a large number of previously failed attempts.
      This is useful to avoid sending reminders for an event after the
      event started.
      There is no expiry by default.
   `max_attempts`:
      maximum number of attempts (number of retries + 1).
      An attempt is failed if a job ends with an exception.
      If an attempt fails and a retry is allowed, the job is rescheduled
      one hour in the future (from now rather than from the scheduled time).
*)
val schedule_job :
  ?ignore_if_exists:bool ->
  ?expiry:Worker_t.timestamp ->
  ?max_attempts:int ->
  Worker_jobid.t ->
  Worker_t.timestamp (* start time, i.e. when the job should run *) ->
  string (* name of the handler that should handle the job data *) ->
  json (* job data (JSON) *) ->
  Worker_t.job Lwt.t

(*
   Replace an existing job with a new specification and new parameters
   or create a new one.

   All job parameters as reset and the internal counter for
   the number of attempts is reset to zero.

   See `schedule_job` for scheduling a job for the first time.
   The meaning of the options is the same as for `schedule_job`.
*)
val reschedule_job :
  ?expiry:Worker_t.timestamp ->
  ?max_attempts:int ->
  Worker_jobid.t ->
  Worker_t.timestamp ->
  string ->
  json ->
  Worker_t.job Lwt.t

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
     allows the job to be rescheduled at some future date
     by the handler itself.

     If the handler raises an exception, the job is removed from the table
     unless it is scheduled for a retry.
  *)
