val ( *@ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val ( @$ ) : ('a -> 'b) -> 'a -> 'b
val id : 'a -> 'a
val round : float -> int
val cons : 'a -> 'a list -> 'a list
val ( ++ ) : string -> string -> string
module Either :
  sig
    type ('a, 'b) t = Left of 'a | Right of 'b
    val either : ('a -> 'b) -> ('c -> 'b) -> ('a, 'c) t -> 'b
    val lefts : ('a, 'b) t list -> 'a list
    val rights : ('a, 'b) t list -> 'b list
    val partitionEithers : ('a, 'b) t list -> 'a list * 'b list
    val fromLeft : ('a, 'b) t -> 'a
    val fromRight : ('a, 'b) t -> 'b
    val isRight : ('a, 'b) t -> bool
    val isLeft : ('a, 'b) t -> bool
  end
module EitherMonad :
  sig
    val bind :
      ('a, 'b) Either.t -> ('b -> ('a, 'c) Either.t) -> ('a, 'c) Either.t
    val return : 'a -> ('b, 'a) Either.t
    val ( >>= ) :
      ('a, 'b) Either.t -> ('b -> ('a, 'c) Either.t) -> ('a, 'c) Either.t
    val ( >> ) : ('a, 'b) Either.t -> ('a, 'c) Either.t -> ('a, 'c) Either.t
    val join : ('a, ('a, 'b) Either.t) Either.t -> ('a, 'b) Either.t
    val fmap : ('a -> 'b) -> ('c, 'a) Either.t -> ('c, 'b) Either.t
    val mzero : (string, 'a) Either.t
    val mplus : ('a, 'b) Either.t -> ('a, 'b) Either.t -> ('a, 'b) Either.t
    val msum : (string, 'a) Either.t list -> (string, 'a) Either.t
    val guard : bool -> (string, unit) Either.t
    val liftM : ('a -> 'b) -> ('c, 'a) Either.t -> ('c, 'b) Either.t
    val liftM2 :
      ('a -> 'b -> 'c) ->
      ('d, 'a) Either.t -> ('d, 'b) Either.t -> ('d, 'c) Either.t
    val ap :
      ('a, 'b -> 'c) Either.t -> ('a, 'b) Either.t -> ('a, 'c) Either.t
    val sequence : ('a, 'b) Either.t list -> ('a, 'b list) Either.t
  end
val failwithf : ('a -> 'b, unit, string, 'c) format4 -> 'a -> 'b
val fromSome : 'a option -> 'a
val isSome : 'a option -> bool
val isNone : 'a option -> bool
val lstrip : string -> string
val splitChar : string -> char -> string list
val median : 'a list -> 'a
val stat : float list -> string
val readLines : in_channel -> string list
val timedShell : int -> string -> (Unix.process_status, string list) Either.t
module Table :
  sig
    module MappedList :
      sig
        type t = (int, unit -> unit) Hashtbl.t
        val create : unit -> ('a, 'b) Hashtbl.t
        val new_id : unit -> int
        val new_property :
          unit ->
          ((int, unit -> unit) Hashtbl.t -> 'a -> unit) *
          ((int, unit -> 'b) Hashtbl.t -> 'a option)
      end
    val get : 'a -> 'b * ('a -> 'c) -> 'c
    val set : 'a -> ('a -> 'b -> 'c) * 'd -> 'b -> 'c
    val create : unit -> ('a, 'b) Hashtbl.t
    val newslot :
      unit ->
      ((int, unit -> unit) Hashtbl.t -> 'a -> unit) *
      ((int, unit -> 'b) Hashtbl.t -> 'a option)
  end
module MaybeMonad :
  sig
    val bind : 'a option -> ('a -> 'b option) -> 'b option
    val return : 'a -> 'a option
    val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
    val ( >> ) : 'a option -> 'b option -> 'b option
    val join : 'a option option -> 'a option
    val fmap : ('a -> 'b) -> 'a option -> 'b option
    val mzero : 'a option
    val mplus : 'a option -> 'a option -> 'a option
    val guard : bool -> unit option
    val liftM : ('a -> 'b) -> 'a option -> 'b option
    val liftM2 : ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
    val liftM3 :
      ('a -> 'b -> 'c -> 'd) ->
      'a option -> 'b option -> 'c option -> 'd option
    val ap : ('a -> 'b) option -> 'a option -> 'b option
    val sequence : 'a option list -> 'a list option
    val replicateM : int -> 'a option -> 'a list option
    val msum : 'a option list -> 'a option
    val msumRmap : 'a list -> ('a -> 'b option) -> 'b option
  end
val parseCmd : string -> string * string array
val matches : ?negative:bool -> string -> string list -> string list
val allsubmatches : string -> string list -> int list -> string list list
val startsWith : hay:string -> needle:string -> bool
val chop : int -> string -> string
val months : string array
val ppTime : ?local:bool -> float -> string
val chopChar : char -> string -> string
val chopString : small:string -> large:string -> string
val chophome : string -> string
val funPower : int -> ('a -> 'a) -> 'a -> 'a
