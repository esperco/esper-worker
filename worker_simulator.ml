(*
   Simulated job scheduler, running jobs sequentially but without waiting
   for the scheduled time.

   This is a naive implementation suitable for testing.
*)

open Log
open Worker_t

(*
   Jobs sorted by ascending timestamp
   (date after right after which the job should run).
*)
let jobs = ref []

let current_time = ref (Util_time.now ())

let update_current_time () =
  match !jobs with
  | [] ->
      ()
  | job :: _ ->
      let t = job.start in
      if Util_time.compare !current_time t < 0 then
        current_time := t

let reset_current_time () =
  current_time := Util_time.now ()

(*
   The current time is the start date of the first job, unless it's already
   in the past.
*)
let now () =
  update_current_time ();
  !current_time

let sort l =
  List.sort (fun a b -> Util_time.compare a.start b.start) l

let job_exists jobid =
  BatList.exists (fun x -> x.jobid = jobid) !jobs

let remove_job jobid =
  jobs := BatList.filter (fun x -> x.jobid <> jobid) !jobs

let add_job job =
  remove_job job.jobid;
  jobs := sort (job :: !jobs)

let get_job jobid =
  try Some (List.find (fun x -> x.jobid = jobid) !jobs)
  with Not_found -> None

let run_due_jobs run_job =
  let now = now () in
  let due_jobs =
    BatList.filter (fun job ->
      Util_time.compare job.start now <= 0
    ) !jobs
  in
  Lwt_list.iter_s (fun job ->
    logf `Info "Run job %s, pretending the date is %s"
      (Worker_jobid.to_string job.jobid)
      (Util_time.to_string now);
    run_job job
  ) due_jobs

open Lwt
class scheduler : Worker.scheduler =
  object
    method is_real = false
    method now () = now ()
    method get_job jobid = return (get_job jobid)
    method job_exists jobid = return (job_exists jobid)
    method add_job job = add_job job; return ()
    method remove_job jobid = remove_job jobid; return ()

    method update_job :
      'a.
         Worker_t.jobid ->
         (Worker_t.job option -> (Worker_t.job option * 'a) Lwt.t) ->
         'a Lwt.t =
      fun jobid f ->
        f (get_job jobid) >>= fun (opt_job, result) ->
        (match opt_job with
         | None -> ()
         | Some job -> add_job job);
        return result

    method run_due_jobs run_job = run_due_jobs run_job
  end

(* Replace the regular scheduler with the simulated one. *)
let activate () =
  logf `Info "Activating fake job scheduler";
  Worker.scheduler := new scheduler

(*
   Run all scheduled jobs in a correct order, advancing the clock
   as much as necessary between steps.

   The `when_cycle_done` function is run at the end of each cycle,
   allowing user code to react to new state of the system.
*)
let run_all_scheduled_jobs ?(when_cycle_done = fun t -> return ()) () =
  let rec loop () =
    if !jobs <> [] then (
      let t = now () in
      logf `Info "New round of jobs using fake scheduler at pretend date %s"
        (Util_time.to_string t);
      Worker.run_due_jobs () >>= fun () ->
      when_cycle_done t >>= fun () ->
      loop ()
    )
    else
      return ()
  in
  assert (not !Worker.scheduler#is_real);
  logf `Info "Running all fake-scheduled jobs.";
  reset_current_time ();
  loop () >>= fun () ->
  logf `Info "Done running all fake-scheduled jobs.";
  return ()
