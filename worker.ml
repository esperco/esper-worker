open Log
open Lwt
open Worker_t

let add_job job =
  Worker_access.Job.put job.jobid job

let remove_job jobid =
  Worker_access.Job.delete jobid

let create_job start ?expiry ?(do_not_retry = false) action =
  let jobid = Jobid.make () in
  let job = {
    jobid;
    start;
    expiry;
    do_not_retry;
    attempts = 0;
    action;
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
    | Some t when Util_time.Op.(>) start t -> true
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

(* Avoid running an email sync for a team if any of the users
   are on another team that is currently being email synced. *)
let run_email_sync_job action_handler job teamid =
  let open Api_t in
  User_perm.get_team teamid >>= fun team ->
  let user_keys =
    List.map (fun uid ->
      "EMAIL_SYNC_" ^ Uid.to_string uid
    ) (team.team_executive :: team.team_assistants)
  in
  Redis_mutex.with_mutexes user_keys (fun () ->
    remove_job job.jobid >>= fun () ->
    action_handler job.action >>= fun () ->
    logf `Info "Job completed: %s" (Worker_j.string_of_job job);
    return ()
  )

let run_job action_handler job =
  catch
    (fun () ->
      match job.action with
      | `Email_sync teamid ->
          run_email_sync_job action_handler job teamid
      | _ ->
          remove_job job.jobid >>= fun () ->
          action_handler job.action >>= fun () ->
          logf `Info "Job completed: %s" (Worker_j.string_of_job job);
          return ()
    )
    (fun e ->
      let s = string_of_exn e in
      logf `Error "Job %s failed with exception %s"
        (Worker_j.string_of_job job) s;
      remove_job job.jobid >>= fun () ->
      if job.do_not_retry then return_unit
      else maybe_retry_later job
    )

let get_oldest () =
  let now = Util_time.now () in
  Worker_access.Job.get_page
    ~direction: `Asc
    ~max_ord: now
    ~max_count: 1000
    ()

let rec run_all action_handler =
  get_oldest () >>= function
  | [] -> return ()
  | l ->
      let jobs = BatList.map (fun (jobid, job, t) -> job) l in
      Lwt_list.iter_p (run_job action_handler) jobs >>= fun () ->
      run_all action_handler
