(*
   System for scheduling jobs at specific times in the future.
*)

open Printf
open Log
open Lwt
open Worker_t

type json = string

let get_job jobid =
  Worker_access.Job.get jobid

let job_exists jobid =
  Worker_access.Job.exists jobid

let add_job job =
  Worker_access.Job.put job.jobid job

let update_job jobid f =
  Worker_access.Job.update jobid f

let remove_job jobid =
  Worker_access.Job.delete jobid

let schedule_job_gen
    ~reschedule
    ?expiry ?(max_attempts = 5)
    jobid start job_type job_spec_json =
  update_job jobid (function
    | Some _ when not reschedule ->
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

let schedule_job ?expiry ?max_attempts jobid start job_type job_spec_json =
  schedule_job_gen
    ~reschedule:false
    ?expiry ?max_attempts jobid start job_type job_spec_json

let reschedule_job ?expiry ?max_attempts jobid start job_type job_spec_json =
  schedule_job_gen
    ~reschedule:true
    ?expiry ?max_attempts jobid start job_type job_spec_json

let maybe_retry_later job0 =
  let now = Util_time.now () in
  let start = Util_time.add now 3600. in (* retry in one hour *)
  let attempts = job0.attempts + 1 in
  let job = { job0 with start; attempts } in
  let expired =
    match job.expiry with
    | Some t when Util_time.(>) start t -> true
    | _ -> false
  in
  if expired || attempts >= job0.max_attempts then (
    (* give up *)
    logf `Error "Giving up on job %s" (Worker_j.string_of_job job);
    remove_job job0.jobid
  )
  else
    (* retry later *)
    add_job job

let job_handlers = Hashtbl.create 10

let register_job_handler job_type job_handler =
  if Hashtbl.mem job_handlers job_type then
    invalid_arg (
      sprintf
        "Worker.register_job_handler: a handler for job type %s is \
         already registered."
        job_type
    )
  else
    Hashtbl.add job_handlers job_type job_handler

let get_job_handler job_type =
  try Some (Hashtbl.find job_handlers job_type)
  with Not_found -> None

let run_job job =
  let jobid = job.jobid in
  catch
    (fun () ->
       remove_job jobid >>= fun () ->
       let job_type, job_spec = job.action in
       let job_handler =
         match get_job_handler job_type with
         | Some job_handler ->
             job_handler
         | None ->
             failwith ("Unknown job type: " ^ job_type)
       in
       Cloudwatch.time "wolverine.worker.job" (fun () ->
         job_handler jobid job_spec
       ) >>= fun may_remove_job ->
       logf `Info "Job completed: %s" (Worker_j.string_of_job job);
       if may_remove_job then
         remove_job jobid
       else
         return ()
    )
    (fun e ->
       let s = string_of_exn e in
       logf `Error "Job %s failed with exception %s"
         (Worker_j.string_of_job job) s;
       maybe_retry_later job
    )

let run_all () =
  Worker_access.Job.iter ~max_ord: (Util_time.now ()) (fun (jobid, job, t) ->
    run_job job
  )
