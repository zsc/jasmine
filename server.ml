open Printf
open Utils
open Jasmine
open Either
open Task
open Machine

let work t x =
  let ot = Unix.gettimeofday () in
  track 2 "%s begins %s" x.ip (Task.show t.tcmd);
  let r = timeout (float t.ttime) (fun () -> Task.exec x t.ttime t.tcmd) () in
  r,x,Unix.gettimeofday ()-.ot

def stats (xs) & add_stat(x) = stats (x::xs)
or stats (xs) & query_stats() = stats(xs) & reply xs to query_stats

def task({tpattern=OnlyX86} as t) & slave ({typ = X86} as x) =
  let r,x,dt = work t x in
  reply (r,x) to task & reply to slave & add_stat (x,dt) 
or task({tpattern=OnlyLoongson} as t) & slave ({typ = Loongson _} as x) =
  let r,x,dt = work t x in
  reply (r,x) to task & reply to slave & add_stat (x,dt) 
or task({tpattern=OnlyLoongson2F} as t) 
    & slave ({typ = Loongson (LS2F,_)} as x) =
  let r,x,dt = work t x in
  reply (r,x) to task & reply to slave & add_stat (x,dt) 

def slaves(xs) & register(x) =
       slaves(x::elim_slave x xs)
     & (track 1 "%s becomes my slave" x.ip;reply to register)
     & begin Join.Site.at_fail x.site (def rm() =
        track 1 "%s flees\n" x.ip;
        remove(x) in rm); 0 end
    or slaves(xs) & remove(x) =
       slaves(elim_slave x xs)
    or slaves(xs) & list_slaves () = slaves(xs) & reply xs to list_slaves

let () =
  let host = try Sys.argv.(1) with _ -> "10.3.0.100" in 
  let port = try int_of_string Sys.argv.(2) with _ -> 12345 in
  spawn slaves [] & stats [];
  runserver host port [
    "register",Obj.magic (register : slave -> unit)
    ;"reportIdle",Obj.magic (slave : slave -> unit)
    ;"list_slaves",Obj.magic (list_slaves : unit -> slave list)
    ;"stats",Obj.magic(query_stats : unit -> (slave * float) list)
    ;"task",Obj.magic(task : Task.t -> command_result option * slave)
    ];
  if not !Sys.interactive then serve ()