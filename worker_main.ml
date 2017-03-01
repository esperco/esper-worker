(*
   Usually this program should be linked with -linkall to ensure
   that the registration of the job handlers takes place
   even if the modules containing the initialization code
   are not referenced anywhere.
*)

open Lwt

let pid_file = "/var/log/wolverine/worker.pid"

(*
   We check the process ID of the previous worker to determine whether
   this worker should start.
*)
let main ~offset =
  Cmdline.parse_options ~offset [];
  let job =
    Apputil_worker.previous_worker_is_still_running
      ~max_age:600. pid_file >>= function
    | true ->
        Log.logf `Info "Previous worker is still running. Aborting.";
        return ()
    | false ->
        Apputil_worker.save_pid pid_file;
        Worker.run_due_jobs ()
  in
  Util_lwt_main.run job
