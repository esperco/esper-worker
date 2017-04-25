(*
   Run a single job remotely or locally.
*)

open Printf
open Lwt
open Log
open Worker_t

type json = string
type job_status = Api_t.job_status

let handlers = Hashtbl.create 10

let register_handler job_type job_handler =
  if Hashtbl.mem handlers job_type then
    invalid_arg (
      sprintf
        "Worker.register_job_handler: a handler for job type %s is \
         already registered."
        job_type
    )
  else
    Hashtbl.add handlers job_type job_handler

let get_handler job_type =
  try Some (Hashtbl.find handlers job_type)
  with Not_found -> None

(*
   Run job locally.
*)
let run job =
  catch
    (fun () ->
       let job_type, job_spec = job.action in
       let job_handler =
         match get_handler job_type with
         | Some job_handler ->
             job_handler
         | None ->
             failwith ("Unknown job type: " ^ job_type)
       in
       Cloudwatch.time "wolverine.worker.job" (fun () ->
         job_handler job.jobid job_spec
       )
    )
    (fun e ->
       let s = string_of_exn e in
       logf `Error "Job %s failed with exception %s"
         (Worker_j.string_of_job job) s;
       return `Failed
    )
