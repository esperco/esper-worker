open Log
open Lwt
open Worker_t

let get_job jobid =
  Worker_access.Job.get jobid

let job_exists jobid =
  Worker_access.Job.exists jobid

let add_job job =
  Worker_access.Job.put job.jobid job

let remove_job jobid =
  Worker_access.Job.delete jobid

let schedule_job
    ?expiry ?(do_not_retry = false)
    jobid start action_name action_json =
  let job = {
    jobid;
    start;
    expiry;
    do_not_retry;
    attempts = 0;
    action = (action_name, action_json);
  } in
  add_job job >>= fun () ->
  return job

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
  if expired || attempts > 100 then (
    (* give up *)
    logf `Error "Giving up on job %s" (Worker_j.string_of_job job);
    return ();
  )
  else
    (* retry later *)
    add_job job

let run_job action_handler job =
  catch
    (fun () ->
      remove_job job.jobid >>= fun () ->
      let action_name, action_json = job.action in
      Cloudwatch.time "wolverine.worker.job" (fun () ->
        action_handler action_name action_json
      ) >>= fun () ->
      logf `Info "Job completed: %s" (Worker_j.string_of_job job);
      return ()
    )
    (fun e ->
      let s = string_of_exn e in
      logf `Error "Job %s failed with exception %s"
        (Worker_j.string_of_job job) s;
      if job.do_not_retry then return_unit
      else maybe_retry_later job
    )

let run_all action_handler =
  Worker_access.Job.iter ~max_ord: (Util_time.now ()) (fun (jobid, job, t) ->
    run_job action_handler job
  )
