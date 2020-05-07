open Printf
open Utils
open Either
open EitherMonad

module Machine = struct
	type minorVersion =
	  | LS2E
	  | LS2F
	type memory =
	  | M512
	  | M1G
	type typ =
	  | X86
	  | Loongson of minorVersion * memory
end
open Machine

let debug = ref 2
let track level a =
    let aux level s = 
      if level < !debug then Join.debug "" "%s" s in
    Printf.ksprintf (aux level) a

open Bigarray
type filename = string
type byte_array = (int, int8_unsigned_elt, c_layout) Array1.t
type command_result = (Unix.process_status,string list) Either.t
type ('a,'b) client = 
     { ip : string;
       site : Join.Site.t;
       typ : typ;
       timedShell : (int * string) -> command_result;
       fetch : (('a,'b) client* filename* filename) -> command_result;
       content : filename -> byte_array;
       kill : unit -> unit;
       do_ace : (int * ('a,'b) client* filename* filename) -> command_result;
      }

type slave = ((int*string),string list option) client

let elim_slave x l = List.filter 
    (fun x' -> not (Join.Site.equal x.site x'.site)) l

module Task = struct
    type cmd = 
      | TFetch of slave * filename* filename
      | TDo_Ace of int * slave * filename* filename
      | TCmd of string
    type pattern =
      | OnlyLoongson
      | OnlyX86
      | OnlyLoongson2F
    type shellCommand = string
    type t = {
        tpattern : pattern;
        ttime : int;
        tcmd : cmd list
      }
    let show_cmd = function
      | TFetch (slave,fn,fn') -> sprintf "copy %s from %s to %s" fn slave.ip fn'
      | TDo_Ace (t,slave,fn,fn') -> 
        sprintf "do_ace for %s from %s to %s in %d" fn slave.ip fn' t
      | TCmd cmd -> cmd
    let show l = String.concat "\n" (List.map show_cmd l)
    let exec slave t l = 
	    let aux = function
	      | TFetch (src,fn,fn') -> slave.fetch(src,fn,fn')
        | TDo_Ace (t,src,fn,fn') -> slave.do_ace(t,src,fn,fn')
	      | TCmd cmd -> slave.timedShell(t, cmd) in
      let rec work = function
        | [] -> Right []
        | x::xs ->
          match aux x with
            | Right _ -> work xs
            | Left y -> Left y in
      work l
    let default = {
      tpattern = OnlyX86;
      ttime = 10;
      tcmd = [TCmd "uname -a;sleep 4"]
    }
end

def async(_) = 0

module BufferedCollector = struct
  type ('a, 'b) t =
      { enter: unit -> unit; leave: 'a Join.chan;
        wait: unit -> 'b; finished: unit Join.chan; }
  let create n combine init =
    def state(n,_::zs,r) & enter() = state(n+1,zs,r) & reply () to enter
    or state(n,zs,r) & leave(v) = state(n-1,()::zs,combine v r)
    or state(0,_,r) & wait() & finished() = reply r to wait in
    spawn state(0,Array.to_list (Array.make n ()),init);
    { enter=enter ; leave=leave ; wait=wait; finished=finished ; }
end
    
let serve () =
    def x () & y () = reply to x in x ()

let timedExec t cmd =
   let prog,args = parseCmd cmd in 
   let id, inchan = JoinProc.open_in prog args in
   def result(r) & wait () & ok () = reply r to wait
   or kill() & wait () & ok() =
      begin try Unix.kill id Sys.sigkill with _ -> () end ;
      reply None to wait in
   spawn begin
     ok() &
     (Thread.delay t; kill ()) &
    let r = Some (readLines inchan) in
       close_in inchan ; 
     match snd (Unix.waitpid [] id) with
     | Unix.WEXITED 0 ->
       result(r)
     | _ -> 0
   end;
   wait

let getCpuRatio () =
  match
    (timedShell 1 "top -b -n 3 -d 0.1|grep Cpu" >>= fun l ->
    return @$ (median @$ List.map (float_of_string*@List.hd) @$ 
        allsubmatches "Cpu(s):[^0-9]*\\([^%]*\\)%.*us.*" l [0]) /. 100.)
  with Right x -> x
    | Left _ -> failwith "getCpuRatio" 

let getCpuNumber () =
  match timedShell 1 "cat /proc/cpuinfo |grep processor|wc -l" with
    | Right [s] -> int_of_string s
    | _ -> failwith "getCpuNumber" 
    
let getLoads () =
  timedShell 1 "uptime" >>= fun l ->
    match List.map (float_of_string*@lstrip) @$ List.hd @$
        allsubmatches ".*load average:\\([^,]*\\),\\([^,]*\\),\\([^,]*\\).*" l [0;1;2]
    with [a;b;c] -> return (a,b,c)
        | _ -> failwith "getLoads" 

let getIP () =
  (* if "127.0.0.1" is returned, check /etc/hosts to see if hostname is there *)
  Unix.string_of_inet_addr (Join.Site.get_local_addr())

let getMemory () =
  match
    (timedShell 1 "cat /proc/meminfo|grep MemTotal" >>= fun l -> 
      return @$ int_of_string @$ List.hd @$ List.hd @$ 
        allsubmatches "MemTotal:[^0-9]*\\([0-9]*\\).*" l [0])
  with Right x -> x
    | Left _ -> failwith "getMemory"

let getMinorVersion () =
  match
    (timedShell 1 "cat /proc/cpuinfo|grep model" >>= fun l ->
    return @$ match  List.hd @$ List.hd @$ 
            allsubmatches ".*:.*V\\([0-9\\.]*\\).*FPU.*" l [0] with
        | "0.2" -> LS2E
        | "0.3" -> LS2F
        | x -> failwith x)
  with Right x -> x
    | Left _ -> failwith "getMinorVersion"
                  
let getTyp () =
  match 
    (timedShell 1 "uname -m" >>= fun l ->
      let s = List.hd l in
      return @$ if Str.string_match (Str.regexp "mips.*") s 0 then
        Loongson (getMinorVersion (), 
          if getMemory () > 1000000 then M1G else M512)
      else if Str.string_match (Str.regexp "i.86") s 0 then X86 else
            failwith "getTyp")
  with Right x -> x
    | Left _ -> failwith "getTyp"

let predictLoad (one,five,fifteeen) =
  (4. *. one +. 2. *. five +. fifteeen) /. 7.
  
let runserver host port services =
  List.iter (fun (a,b) -> Join.Ns.register Join.Ns.here a b) services;
  Join.Site.listen (
    Unix.ADDR_INET (Join.Site.get_local_addr(), port));
  track 1 "server started at %s:%d\n" host port;
  spawn async (serve ())
  
let getServer server_host server_port = 
  let server_addr = Unix.gethostbyname server_host in
  Join.Site.there (
    Unix.ADDR_INET(server_addr.Unix.h_addr_list.(0),server_port))

let checkIdle self cpu_number reportIdle =
  let checkLoadInterval = 20.0 in
  let checkCpuRatioInterval = 3.0 in
  def checkIdle () =
    match self.typ with
(*      | _ ->                                                          *)
(*        begin                                                         *)
(*              match getLoads () with                                  *)
(*                | Right loads ->                                      *)
(*                    if predictLoad loads < float (2 * cpu_number) then*)
(*                      (reportIdle (self);checkIdle ())                *)
(*                    else                                              *)
(*                      (Thread.delay checkLoadInterval;checkIdle ())   *)
(*                | Left _ ->                                           *)
(*		              track 1 "checkIdle failed for %s" self.ip;          *)
(*		              Thread.delay checkLoadInterval;                     *)
(*		              checkIdle ()                                        *)
(*        end                                                           *)
      | _ ->
				begin
				  let ratio = getCpuRatio () in
				  if float cpu_number *. ratio < 0.9 *. float cpu_number then
				    (reportIdle (self); checkIdle ())
				  else
				    (Thread.delay checkCpuRatioInterval; checkIdle ())
				end
     in
    checkIdle

let timeout t f x =
  def wait() & finished(r) = reply Some r to wait
  or  wait() & timeout() = reply None to wait & killed () 
  or finished(_) & killed () = 0 in
   spawn begin
    let r = f x in finished(r) &
     (Thread.delay t; timeout())
   end ;
   wait ()