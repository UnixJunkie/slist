
module RNG = Random.State

let rng = RNG.make_self_init ()

(* type ('a, 'b) elt *)

(* type t *)

(* FBR: instead of using cmp, we should use a functor *)

(* (\* probability constant *\)
 * let p = 0.5 *)

let max_level = 32

let random_level () =
  let level = ref 0 in
  while RNG.bool rng && !level < max_level do
    incr level
  done;
  !level

let create () =
  failwith "not implemented yet"

let of_list _l =
  failwith "not implemented yet"

let to_list _t =
  failwith "not implemented yet"

let add _cmp _l _x =
  failwith "not implemented yet"

let at _l _i =
  failwith "not implemented yet"

let find _cmp _l _x =
  failwith "not implemented yet"

let remove _cmp _l _x =
  failwith "not implemented yet"

let length _l =
  failwith "not implemented yet"
