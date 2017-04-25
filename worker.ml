(*
   System for scheduling jobs at specific times in the future.
   See worker.mli.
*)

open Printf
open Log
open Lwt
open Worker_t

let default_max_attempts = 5

type scheduling_mode = [
  | `New
  | `Ignore_if_exists
  | `Reschedule
]

type job_status = [
  | `OK
  | `Rescheduled
  | `Failed
]

let string_of_job_status : job_status -> string = function
  | `OK -> "OK"
  | `Rescheduled -> "Rescheduled"
  | `Failed -> "Failed"

class scheduler =
  let now () = Util_time.now () in
  object
    method is_real = true

    method now () =
      Util_time.now ()

    method get_job jobid =
      Worker_access.Job.get jobid

    method job_exists jobid =
      Worker_access.Job.exists jobid

    method add_job job =
      Worker_access.Job.put job.jobid job

    method remove_job jobid =
      Worker_access.Job.delete jobid

    method update_job :
      'a. jobid ->
          (job option ->
          (job option * 'a) Lwt.t) ->
          'a Lwt.t =
      fun jobid f ->
        Worker_access.Job.update jobid f

    method run_due_jobs run_job =
      Worker_access.Job.iter
        ~max_ord: (now ())
        (fun (jobid, job, t) ->
           run_job job
        )
  end

let real_scheduler = new scheduler

(* Could be substituted by another scheduling engine for testing purpose *)
let scheduler = ref real_scheduler

let now () = !scheduler#now ()
let time () = Util_time.to_float (now ())

let get_job jobid = !scheduler#get_job jobid

let add_job job =
  Log.debug (fun () ->
    sprintf "Schedule job %s for %s"
      (Worker_jobid.to_string job.jobid)
      (Util_time.to_string job.start)
  );
  !scheduler#add_job job

let remove_job jobid =
  Log.debug (fun () ->
    sprintf "Remove job %s"
      (Worker_jobid.to_string jobid)
  );
  !scheduler#remove_job jobid

let now () = !scheduler#now ()

let schedule_job
    ?expiry
    ?(max_attempts = default_max_attempts)
    (mode : scheduling_mode)
    jobid start job_type job_spec_json =

  let ignore_if_exists, reschedule =
    match mode with
    | `New ->
        false, false
    | `Ignore_if_exists ->
        true, false
    | `Reschedule ->
        true, true
  in
  !scheduler#update_job jobid (function
    | Some job when not reschedule ->
        if ignore_if_exists then
          return (None, job)
        else
          failwith
            (sprintf
               "Worker.schedule: a job with ID %s is already scheduled"
               (Worker_jobid.to_string jobid)
            )
    | _ ->
        let job = {
          jobid;
          start;
          expiry;
          max_attempts;
          attempts = 0;
          action = (job_type, job_spec_json);
        } in
        return (Some job, job)
  )

let is_expired_at job t =
  match job.expiry with
  | Some expiry ->
      if Util_time.(<) t expiry then
        false
      else
        true
  | None ->
      false

let maybe_retry_later job0 =
  let now = !scheduler#now () in
  let start = Util_time.add now 3600. in (* retry in one hour *)
  let attempts = job0.attempts + 1 in
  let job = { job0 with start; attempts } in
  let expired = is_expired_at job start in
  if expired || attempts >= job0.max_attempts then (
    (* give up *)
    logf `Error "Giving up on job %s" (Worker_j.string_of_job job);
    remove_job job0.jobid
  )
  else
    (* retry later *)
    add_job job

let run_remote_job jobid =
  let url = Uri.of_string (App_path.Webhook.job_url jobid) in
  Util_http_client.get url >>= function
  | `OK, headers, body ->
      let {Api_t.job_status} = Api_j.job_status_response_of_string body in
      return job_status
  | status, headers, body ->
      let msg =
        sprintf "Failed worker call: status %s, body: %s"
          (Cohttp.Code.string_of_status status)
          body
      in
      Apputil_error.report_error "Failed worker call" msg >>= fun () ->
      return `Failed

let order_job job =
  let jobid = job.jobid in
  if is_expired_at job (now ()) then (
    logf `Error "Job has expired, not running it: %s"
      (Worker_j.string_of_job job);
    remove_job jobid
  )
  else
    run_remote_job job.jobid >>= fun job_status ->
    logf `Info "Job ended with status %s: %s"
      (string_of_job_status job_status)
      (Worker_j.string_of_job job);
    match job_status with
    | `OK ->
        remove_job jobid
    | `Rescheduled ->
        return ()
    | `Failed ->
        maybe_retry_later job

let run_due_jobs () =
  !scheduler#run_due_jobs order_job
