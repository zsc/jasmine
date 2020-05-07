open Printf
open Utils
open Jasmine
open Either
open Task
module Col = JoinCount.Dynamic

let () =
  let server_host = try Sys.argv.(1) with _ -> "10.3.0.100" in 
  let server_port = try int_of_string Sys.argv.(2) with _ -> 12345 in
  let server = getServer server_host server_port in
  let ns = Join.Ns.of_site server in
  let list_slaves = (Join.Ns.lookup ns "list_slaves" : unit -> slave list) in
  let slaves = list_slaves () in
  let show res f =
    match res with
      | Left _ -> "error\n"
      | Right l -> String.concat "\n" @$ List.map f l in
  let col = Col.create cons [] in
  List.iter (fun slave ->
    col.Col.enter ();
    let id = sprintf "%s:" slave.ip in
    spawn (col.Col.leave(id ^
    show (slave.timedShell (2,"uptime")) (fun s -> chop (String.length id) s^"\n") ^
    show (slave.timedShell (2,"top -b -n 1 -d 0.1|head|tail -n 3|head -n 2")) 
        (fun s -> String.sub s 0 (min (String.length s) 80)))) 
      ) slaves;
  spawn col.Col.finished();
  List.iter print_endline @$ List.sort compare @$ col.Col.wait()
    
    