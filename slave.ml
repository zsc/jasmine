open Utils
open Jasmine
open Bigarray
open Either

(* file service *)
def content (fn) =
  let fd = Unix.openfile fn [Unix.O_RDONLY] 0 in
  let str = Array1.map_file fd int8_unsigned c_layout false (-1) in
  Unix.close fd;
  reply str to content
  
def fetch(client,fn,fn') =
  let str = client.content (fn) in
  let len = Array1.dim str in
  (* poor man's munmap *)
  (match Unix.fork () with
    | 0 ->
      let fd' = Unix.openfile fn'
        [Unix.O_RDWR; Unix.O_TRUNC; Unix.O_CREAT] 0o777 in
		  let str' = Array1.map_file fd' int8_unsigned c_layout true len in
			Array1.blit str str';
      Unix.close fd';
      exit 0
    | id -> ignore (Unix.waitpid [] id));
  reply (Right []) to fetch
  
def do_ace(t,client,fn,fn') =
  let cfn = fn'^".gz" in
  let r =
	  match fetch(client,fn,cfn) with
	    | Right _ ->
			  (match Unix.fork () with
			    | 0 -> Unix.execvp "gunzip" [|"gunzip";cfn|]
			    | id -> ignore (Unix.waitpid [] id));
        Unix.chmod fn' 0o700;
	      let res = timedShell t fn' in
        ignore (Thread.create (fun () -> Sys.command ("rm "^fn')) ());
(*        (match Unix.fork () with              *)
(*          | 0 -> Unix.execvp "rm" [|"rm";fn'|]*)
(*          | id -> ());                        *)
        res
	    | Left _ as x -> x in
  reply r to do_ace

let () =
  let parallel = try int_of_string Sys.argv.(1) with _ -> 4 in
  let server_host = try Sys.argv.(2) with _ -> "10.3.0.100" in 
  let server_port = try int_of_string Sys.argv.(3) with _ -> 12345 in
  let server = getServer server_host server_port in
  let ns = Join.Ns.of_site server in
  let register = (Join.Ns.lookup ns "register" : 
    slave -> unit) in
  def wait () & kill () = reply to wait & reply to kill in
  Join.Site.at_fail server (def suicide() =
    track 1 "My master has died, ahh";
    kill();
    0 in suicide);
  let self = {site = Join.Site.here
    ; ip = getIP ()
    ; typ = getTyp ()
    ; timedShell =
      (def p ((t,cmd)) =
        reply (timedShell t cmd) to p in p)
    ; kill = kill
    ; content = content
    ; fetch = fetch
    ; do_ace = do_ace
      } in
  register self;
  let reportIdle =  (Join.Ns.lookup ns "reportIdle" : 
    slave -> unit) in
  let cpu_number = getCpuNumber () in
  for i = 1 to parallel do spawn (checkIdle self cpu_number reportIdle ()) done;
  wait ()
(*  ignore (Unix.waitpid [] (-1))*)