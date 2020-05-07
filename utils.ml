let ( *@) f g = fun x -> f (g x)
let (@$) f x = f x
let id x = x
let round f = int_of_float (floor (f +.0.5))
let cons a b = a::b
let (++) x y = if x="" then y else if y="" then x else x^" "^y

(*  let show = function Right l -> List.iter print_endline l | Left _ -> () in*)
module Either = struct
  type ('a,'b) t = Left of 'a | Right of 'b
  
  let either f g = function
    | Left a -> f a
    | Right b -> g b
  let rec lefts = function
    | [] -> []
    | Left a:: xs -> a:: lefts xs
    | _:: xs -> lefts xs
  let rec rights = function
    | [] -> []
    | Right b:: xs -> b:: rights xs
    | _:: xs -> rights xs
  let partitionEithers l =
    let rec loop s s2 = function
      | [] -> List.rev s, List.rev s2
      | Left a:: xs -> loop (a:: s) s2 xs
      | Right b:: xs -> loop s (b:: s2) xs in
    loop [] [] l
  let fromLeft = function
    | Left x -> x
    | _ -> failwith "fromLeft" 
  let fromRight = function
    | Right x -> x
    | _ -> failwith "fromRight" 
  let isRight = function
    | Right _ -> true
    | _ -> false
  let isLeft x = not (isRight x)
end
module EitherMonad = struct
  open Either
  
  let bind m k = match m with
    | Left a -> Left a
    | Right b -> k b
  let return x = Right x
  let (>>=) = bind
  let (>>) l l2 = bind l (fun _ -> l2)
  let join m = m >>= id
  let fmap f = function
    | Left a -> Left a
    | Right b -> Right (f b)
  let mzero = Left ""
  let mplus x y = match x, y with
    | Left _, b -> b
    | a, _ -> a
  let msum l = List.fold_right mplus l mzero
  let guard flg = if flg then return () else mzero
  let liftM f m = m >>= fun x -> return @$ f x
  let liftM2 f m1 m2 =
    m1 >>= fun x1 ->
        m2 >>= fun x2 ->
            return @$ f x1 x2
  let ap fm m =
    fm >>= fun f ->
        m >>= fun x ->
            return @$ f x
  let sequence ms =
    let k m m' =
      m >>= fun x ->
          m' >>= fun xs ->
              return (x:: xs) in
    List.fold_right k ms (return [])
end

let failwithf fmt a = Printf.kprintf failwith fmt a 
let fromSome = function
  | Some x -> x
  | None -> failwith "fromSome"
let isSome = function
  | Some _ -> true
  | None -> false
let isNone x = not (isSome x)
let lstrip s =
  let n = String.length s in
  let rec work i =
    if i>= n then "" else
      if s.[i]==' ' then work (i+1) else
        String.sub s i (n-i) in
  work 0
let splitChar s c =
  let n = String.length s in
  let rec work i acc =
    if i>=n then List.rev acc else
      try
          let i' = String.index_from s i c in
            work (i'+1) (String.sub s i (i'-i)::acc)
      with Not_found -> List.rev (String.sub s i (n-i)::acc)
  in
  work 0 []
let median l =
  let n = List.length l in
  assert (n<>0);
  List.nth (List.sort compare l) (n/2)

let stat l =
  let sum l = List.fold_left (fun s i -> s+.i) 0. l in
  let totVar avg l = List.fold_left (fun s k -> s+.(k-.avg)*.(k-.avg)) 0. l in
  let n = List.length l in
  let s = sum l in
  let avg = s/.float n in
  if n=0 then "zero length list" else
  Printf.sprintf "len:%d, sum:%.2f, med:%.2f, avg:%.2f, max:%.2f, min:%.2f, std:%.2f" 
    n s (median l) avg (List.fold_left max (-.infinity) l) 
        (List.fold_left min infinity l) (sqrt (totVar avg l /. float n))
let readLines ic =
  let l = ref [] in
  try
    while true do
      l := input_line ic :: !l
    done;
    failwith "readLines"
  with End_of_file -> List.rev !l
  
open Either
let timedShell t cmd =
  let id, (inchan, oc) = JoinProc.open_in_out "sh" @$ [|"sh";"-s" |] in
  let s = "ulimit -t "^string_of_int t^";"^cmd^";" in
  output_string oc s;
  close_out oc;
  let r = readLines inchan in
  close_in inchan ;
  match snd (Unix.waitpid [] id) with
  | Unix.WEXITED 0 ->
    Right r
  | x ->
    Left x
  
module Table = struct
  module MappedList =
    struct
      type t = (int, unit -> unit) Hashtbl.t
      let create () = Hashtbl.create 13
      let new_id : unit -> int =
        let id = ref 0 in
          fun () -> incr id; !id
      let new_property () =
        let id = new_id () in
        let v = ref None in
        let set t x =
          Hashtbl.replace t id (fun () -> v := Some x) in
        let get t =
          try
            (Hashtbl.find t id) ();
            match !v with
                Some x as s -> v := None; s
              | None -> None
          with Not_found -> None
        in
          (set, get)
    end  
  let get t (set, get) = get t
  let set t (set, get) x = set t x
  let create = MappedList.create
  let newslot = MappedList.new_property
end

module MaybeMonad = struct
  let bind m k = match m with
      None -> None
    | Some x -> k x
  let return x = Some x
  let (>>=) = bind
  let (>>) l l2 = bind l (fun _ -> l2)
  let join m = m >>= id
  let fmap f = function
    | None -> None
    | Some x -> Some (f x)
  let mzero = None
  let mplus x y = match x, y with
    | None, b -> b
    | a, _ -> a
  (* let msum l = List.fold_right mplus l mzero *)
  let guard flg = if flg then return () else mzero
  let liftM f m = m >>= fun x -> return @$ f x
  let liftM2 f m m' = m >>= fun x -> m' >>= fun x' -> return @$ f x x'
  let liftM3 f m m' m'' =
    m >>= fun x -> m' >>= fun x' -> m'' >>= fun x'' -> return @$ f x x' x''
  let ap fm m =
    fm >>= fun f ->
        m >>= fun x ->
            return @$ f x
  let sequence ms =
    let k m m' =
      m >>= fun x ->
          m' >>= fun xs ->
              return (x:: xs) in
    List.fold_right k ms (return [])
  let replicateM n m =
    let replicate n e =
        let rec loop stk i = if i = 0 then stk else loop (e:: stk) (i - 1) in
        loop [] n in 
    sequence (replicate n m)
  
  let msum l =
    let rec loop = function
      | [] -> mzero
      | Some x:: xs -> return x
      | None:: xs -> loop xs in
    loop l
  let msumRmap l f =
    let rec loop = function
      | [] -> mzero
      | x:: xs -> let x' = f x in
        begin match x' with 
          | Some _ -> x'
          | None -> loop xs 
        end in
    loop l
end

let parseCmd cmd =
  let l = List.filter ((<>)"") (splitChar cmd ' ') in
  List.hd l, Array.of_list l

let matches ?(negative=false) pattern l =
  let aux s =
    (if negative then not else id) @$
      Str.string_match (Str.regexp pattern) s 0 in
  List.filter aux l

let allsubmatches pattern l sub_ids =
  let rec work acc = function
    | [] -> List.rev acc
    | x::xs ->
        if Str.string_match (Str.regexp pattern) x 0 then
          work ((List.map (fun i -> Str.matched_group (i+1) x) sub_ids)::acc) xs
        else work acc xs in
  work [] l
  
let startsWith ~hay:s ~needle:s'=
  String.length s >= String.length s' && String.sub s 0 (String.length s') = s'
  
let chop n s= String.sub s n (String.length s-n)
let months = [|"Jan.";"Feb.";"Mar.";"Apr.";"May";"Jun.";"Jul.";"Aug.";"Sep.";"Oct.";"Nov.";"Dec." |]
let ppTime ?(local = true) f =
    let tm = (if local then Unix.localtime else Unix.gmtime) f in
    Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
let chopChar c s =
  if s="" then s else if s.[0] == c then chop 1 s else s
let chopString ~small ~large =
  if startsWith ~hay:large ~needle:small then chop (String.length small) large 
  else large
let chophome s =
  let needle = "~/" in
  if startsWith ~needle ~hay:s then chop (String.length needle) s else 
  let needle = Sys.getenv "HOME" in
  if startsWith ~needle ~hay:s then chop (String.length needle) s else s
  
let funPower n f x =
  let rec work i x =
    if i<=0 then x else work (i-1) (f x) in
  assert (n >=0);
  work n x