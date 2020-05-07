open Printf
open Filename
open Utils
open Jasmine
open Either
open Task
open BufferedCollector
type stage =
  | Compile
  | Run
type fault =
  | Timeout
(*  | WrongFile*)
  | Fail
let show_result = function
  | Right _ -> "Success"
  | Left (Compile,Timeout) -> "CompileTimeout"
  | Left (Compile,Fail) -> "CompileFail"
(*  | Left (Compile,WrongFile) -> "CompileWrongFile"*)
  | Left (Run,Timeout) -> "RunTimeout"
  | Left (Run,Fail) -> "RunFail"

let retriesOfSiteFail = ref 0
let retriesOfTimeout = ref 0

let retryIfSiteFail n f =
  let rec work i =
    if i>= n then Left () else
		  try f ()
		  with Join.Exit -> Thread.delay 5.0;incr retriesOfSiteFail;work (i+1) in
  work 0
  
let retryIfTimeout n t f arg =
  let rec work i =
    if i>=n then Left arg else
      let r = timeout t f arg in
      match r with
        | Some x -> x 
        | None -> Thread.delay 5.0;incr retriesOfTimeout;work (i+1) in
  work 0
def ace(compiler,task,jobtimeout,results,dir,flag,makefile) =
  def onepass(files,rights) =
	  let check = function
	    | Right _ -> Right ()
	    | Left (Unix.WSIGNALED 9) -> Left(Run,Timeout)
	    | _ -> Left(Run,Fail) in
	  let work file =
	    let nTries = 3 in
	    let bin = chop_extension file in
	    let dir = dirname file in
	    let base = basename file in
	    let binbase = basename bin in
	    let errfile = bin ^ ".err" in
	    let ctask = {
	      tpattern = OnlyX86;
	      ttime = 20;
	      tcmd =
	        [TCmd (
	        sprintf "rm %s %s.o %s.gz %s 2>/dev/null;" bin bin bin errfile^
	        sprintf "make -C %s CC=%s compile EXTRA_CFLAGS=\"%s\" -f %s TESTNAME=%s C_SRC_FILES=%s 2>%s" 
	          dir compiler flag makefile binbase base errfile);
	        TCmd (sprintf "gzip %s" bin)]
	      } in
	    match retryIfSiteFail nTries @$ fun () -> Right (task (ctask)) with
	    | Right (r,cslave) ->
		    let compileResult = check r in
	      if isRight compileResult then begin
		        let localfilename = Filename.temp_file base ".ace" in
		        let rtask = {
		          tpattern = OnlyLoongson;
		          ttime = 20;
		          tcmd =[TDo_Ace (20,cslave, bin^".gz", localfilename)]
		          } in
		        match retryIfSiteFail nTries @$ fun () -> Right (task (rtask)) with
	            | Right (r,_) ->         
				        let result = check r in
				        track 1 "%s got %s" file (show_result result);
				        track 2 "%s got %s" (Task.show rtask.tcmd) (show_result result);
				        Right (file, show_result result)
	            | Left _ -> Right (file,sprintf "failed %d attempts" nTries)
		      end else begin
		        track 1 "%s got %s" file (show_result compileResult);
		        track 2 "%s got %s" (Task.show ctask.tcmd) (show_result compileResult);
		        Right (file, show_result compileResult)
		      end
	     | Left _ -> Right (file,sprintf "failed %d attempts" nTries) in

	  List.iter (fun file ->
	    results.enter ();
	    spawn (results.leave (retryIfTimeout 3 jobtimeout work file));
	    ) files;
	  track 1 "all spawned";
	  spawn (results.finished ());
    let lefts,newrights = partitionEithers @$ results.wait () in
	  reply (lefts,newrights@rights) to onepass in
    let files = List.filter (not*@Sys.is_directory)
             (fromRight @$ timedShell 10 
               (sprintf "du -a %s|grep '\\.c$'|cut -f2|grep -v embed" dir)) in
    reply funPower 3 onepass (files,[]) to ace

let () =
  let dir = try Sys.argv.(1) with _ -> "~/test_suite/ace/" in
  let flag = try Sys.argv.(2) with _ -> "-O0 -static" in
  let compiler = try Sys.argv.(3) with _ -> "loongcc" in
  let makefile = try Sys.argv.(4) with _ -> "~/test_suite/ace/default.mk" in
  let server_host = try Sys.argv.(5) with _ -> "10.3.0.100" in 
  let server_port = try int_of_string Sys.argv.(6) with _ -> 12345 in
  let server = getServer server_host server_port in
  let jobtimeout = 30. in
  let ns = Join.Ns.of_site server in
  let task = (Join.Ns.lookup ns "task" : Task.t -> command_result * slave) in
  let stats = (Join.Ns.lookup ns "stats" : unit -> (slave*float) list) in
  let results = create 1000 cons [] in
  let otime = Unix.time () in
  track 1 "%s with %s %s starts at %s" dir compiler flag (ppTime otime); 
  let lefts,rights = ace (compiler,task,jobtimeout,results,dir,flag,makefile) in
  print_endline "failed:";
  List.iter print_endline lefts;
  print_endline "succeeded:";
  List.iter (fun (a,b) -> printf "%s\t%s" a b) rights;
  let ntime = Unix.time () in
	track 1 "all done at %s" (ppTime ntime);
  track 1 "duration: %.2fs" (ntime -.otime);
  track 1 "retriesOfSiteFail: %d" !retriesOfSiteFail;
  track 1 "retriesOfTimeout: %d" !retriesOfTimeout;
  Hashtbl.iter (fun ip fl -> track 1 "%s: %s" ip (stat fl)) @$ 
    List.fold_left (fun h (s,t) ->
        Hashtbl.replace h s.ip (t::try Hashtbl.find h s.ip with _ -> []);h) 
            (Hashtbl.create 3) (stats ())