
(* Functional Skip Lists (c) Copyright 2004 Pierre Corbineau *)

module type OrderedType=
sig
  type t
  val compare: t -> t-> int
end

module type S=
sig
  type elt
  type t
  val empty: t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal: t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val print : (elt -> string) -> t -> unit
end

module Make(M:OrderedType) : S with type elt = M.t =
struct

  type elt = M.t (* elements *)
      
  type level=int
  type t = 
      Level of level * t * skip
    | Bottom
  and skip=
      Node of elt * t * skip
    | Last of elt * t

  (* We use a private PRNG state *) 

  let prng_state = Random.State.make_self_init ()

  (* we set max_level to be optimized for sets containing up to 
     [max_int] elements *) 

  let max_level = 32

  (* random integer following a geometric law with parameter 1/p *)

  let rec gen n=
    if n <= max_level &&
      Random.State.int prng_state 2 = 0 
    then gen (n+1)
    else n

  let head = function Node (elt,_,_) | Last (elt,_) -> elt

  let decapitate lvl = function
      Node (_,down,right) -> Level(lvl,down,right)
    | Last (_,down) -> down

  let empty = Bottom

  let is_empty s = s = Bottom 
			 
  let rec mem x = function
      Bottom -> false
    | Level (_,down,right) ->
	let cmp = M.compare x (head right) in
	  cmp = 0 ||
		if cmp > 0 then 
		  mem_skip x right 
		else
		  mem x down 
  and mem_skip x = function
      Node (_,down,right) ->
      	let cmp = M.compare x (head right) in
	  cmp = 0 ||
		if cmp > 0 then 
		  mem_skip x right 
		else
		  mem x down
    | Last (_,down) -> mem x down

  (* splits a set A in subsets < x and > x , present  is true if x is in A *) 
	
  let rec split x = function
      Bottom -> (Bottom,false,Bottom)
    | Level (lvl,down,right) ->
	let cmp = M.compare x (head right) in
	  if cmp = 0 then 
	    down,true,decapitate lvl right
	  else if cmp > 0 then
	    let lskip,present,rset = split_skip lvl x right in 
	      (Level(lvl,down,lskip),present,rset)
	  else 
	    let lset,present,rset=split x down in 
	      (lset,present,Level(lvl,rset,right))
  and split_skip lvl x = function
      Node (elt,down,right) ->
	let cmp = M.compare x (head right) in
	  if cmp = 0 then 
	    Last(elt,down),true,decapitate lvl right
	  else if cmp > 0 then
	    let lskip,present,rset = split_skip lvl x right in 
	      Node(elt,down,lskip),present,rset
	  else 
	    let lset,present,rset = split x down in 
	      Last (elt,lset),present,Level(lvl,rset,right)
    | Last (elt,down) ->
	let lset,present,rset = split x down in 
	  Last (elt,lset),present,rset
	    
  let rec insert lvlx x set =
    match set with
	Bottom -> Level (lvlx,Bottom,Last(x,Bottom))
      | Level(lvl,down,right) ->
	  if lvl < lvlx then
	    let lset,present,rset = split x set in
	      if present then set 
	      else Level(lvlx,lset,Last(x,rset))
	  else
	    let cmp = M.compare x (head right) in
	      if cmp = 0 then set
	      else if cmp > 0 then
		Level(lvl,down,insert_skip lvl lvlx x right)
	      else if lvl = lvlx then
		let lset,present,rset = split x down in
		  if present then set
		  else Level(lvl,lset,Node(x,rset,right)) 
	      else
		Level(lvl,insert lvlx x down,right)
  and insert_skip lvl lvlx x skip = (* assert (lvl >= lvlx); *)
    match skip with 
	Node (elt,down,right) ->
	  let cmp = M.compare x (head right) in
	    if cmp = 0 then skip 
	    else if cmp > 0 then
	      Node (elt,down,insert_skip lvl lvlx x right)
	    else if lvl = lvlx then
	      let lset,present,rset = split x down in
		if present then skip
		else Node (elt,lset,Node (x,rset,right))
	    else
	      Node (elt,insert lvlx x down,right)
      | Last (elt,down) ->
	  if lvl = lvlx then
	    let lset,present,rset = split x down in
	      if present then skip
	      else Node (elt,lset,Last (x,rset))
	  else
	    Last (elt,insert lvlx x down)
  	      
  let add x s = insert (gen 0) x s

  let single lvl x = Level (lvl,Bottom,Last(x,Bottom))

  let singleton x = single (gen 0) x

  let rec merge lset rset = 
    (* merges two sets A and B such that for all x in A and y in B, y > x *)  
    match lset with
	Bottom -> rset
      | Level (llvl,ldown,lright) ->
	  match rset with
	      Bottom -> lset
	    | Level (rlvl,rdown,rright) ->
		if llvl < rlvl then
		  Level (rlvl,merge lset rdown,rright)
		else 
		  Level (llvl,ldown,merge_skip llvl lright rset) 
  and merge_skip llvl lskip rset = 
    match lskip with
	Node (elt,ldown,lright) ->
	  Node (elt,ldown,merge_skip llvl lright rset)
      | Last (elt,ldown) ->
	  match rset with
	      Bottom -> lskip
	    | Level (rlvl,rdown,rright) -> (* assert (llvl >= rlvl); *)
		if llvl = rlvl then
		  Node (elt,merge ldown rdown,rright)
		else
		  Last (elt,merge ldown rset)
		    
  let rec remove x = function
      Bottom -> Bottom
    | Level (lvl,down,right) ->
	let cmp = M.compare x (head right) in
	  if cmp = 0 then 
	    merge down (decapitate lvl right)
	  else if cmp > 0 then 
	    Level(lvl,down,remove_skip lvl x right)
	  else
	    Level(lvl,remove x down,right)
  and remove_skip lvl x = function
      Node (elt,down,right) ->
	let cmp = M.compare x (head right) in
	  if cmp = 0 then 
	    merge_skip lvl (Last(elt,down)) (decapitate lvl right)
	  else if cmp > 0 then 
	    Node (elt,down,remove_skip lvl x right)
	  else
	    Node (elt,remove x down,right)
    | Last (elt,down) ->
	Last (elt,remove x down)

  module type St =
  sig
    type stack
    val unzip : t -> stack
    val zip : stack -> t
    val union : stack -> stack -> stack
    val inter : stack -> stack -> stack
    val diff : stack -> stack -> stack
    val compare : stack -> stack -> int
    val equal : stack -> stack -> bool
    val subset : stack -> stack -> bool
    val filter : (elt -> bool) -> stack -> stack
    val partition : (elt -> bool) -> stack -> stack * stack
  end

  module Stack : St =
  struct 
    type stack=(level*skip) list
	
    let rec push set stack =
      match set with
	  Bottom -> stack
	| Level(lvl,down,right) -> push down ((lvl,right)::stack)
	    
    let unzip s = push s []

    let zip st =
      List.fold_left (fun down (lvl,right) -> Level(lvl,down,right)) Bottom st
	
    let split = function
	[] -> invalid_arg "split"
      | (lvl,skip)::stack -> 
	  match skip with
	      Node(elt,down,right) -> lvl,elt,push down ((lvl,right)::stack) 
	    | Last(elt,down) -> lvl,elt,push down stack
		
    let rec cons_aux lvlx x down st = 
      match st with
	  [] -> [lvlx,Last(x,down)]
	| (lvl,right)::stack ->
	    if lvlx < lvl then 
	      (lvlx,Last(x,down))::st
	    else if lvlx = lvl then 
	      (lvlx,Node(x,down,right))::stack
	    else
	      cons_aux lvlx x (Level(lvl,down,right)) stack
		
    let cons lvlx x st = 
      cons_aux lvlx x Bottom st

    let rec union st1 st2=
      match st1,st2 with
	  [],_ -> st2
	| _,[] -> st1
	| _,_ -> 
	    let lvl1,elt1,cont1 = split st1 in
	    let lvl2,elt2,cont2 = split st2 in
	    let cmp = M.compare elt1 elt2 in 
	      if cmp = 0 then
		cons lvl1 elt1 (union cont1 cont2)
	      else if cmp > 0 then
		cons lvl2 elt2 (union st1 cont2)
	      else 
		cons lvl1 elt1 (union cont1 st2)
		  
    let rec inter st1 st2=
      match st1,st2 with
	  [],_ -> []
	| _,[] -> []
	| _ -> 
	    let lvl1,elt1,cont1 = split st1 in
	    let _,elt2,cont2 = split st2 in
	    let cmp = M.compare elt1 elt2 in 
	      if cmp = 0 then
		cons lvl1 elt1 (inter cont1 cont2)
	      else if cmp > 0 then
		inter st1 cont2
	      else 
		inter cont1 st2
		  
    let rec diff st1 st2=
      match st1,st2 with
	  [],_ -> []
	| _,[] -> st1
	| _ -> 
	    let lvl1,elt1,cont1 = split st1 in
	    let _,elt2,cont2 = split st2 in
	    let cmp = M.compare elt1 elt2 in 
	      if cmp = 0 then
		diff cont1 cont2
	      else if cmp > 0 then
		diff st1 cont2
	      else cons lvl1 elt1 (diff cont1 st2)
		
    let rec compare st1 st2=
      match st1,st2 with
	  [],_ -> -1 
	| _,[] -> 1
	| _ -> 
	    let _,elt1,cont1 = split st1 in
	    let _,elt2,cont2 = split st2 in
	    let cmp = M.compare elt1 elt2 in
	      if cmp = 0 then
		compare cont1 cont2
	      else cmp

    let rec equal st1 st2=
      match st1,st2 with
	  [],[] -> true  
	| [],_ -> false
	| _,[] -> false
	| _ -> 
	    let _,elt1,cont1 = split st1 in
	    let _,elt2,cont2 = split st2 in
	    let cmp = M.compare elt1 elt2 in
	      cmp = 0 && equal cont1 cont2

    let rec subset st1 st2=
      match st1,st2 with
	  [],_ -> true  
	| _,[] -> false
	| _ -> 
	    let _,elt1,cont1 = split st1 in
	    let _,elt2,cont2 = split st2 in
	    let cmp = M.compare elt1 elt2 in
	      (cmp = 0 && subset cont1 cont2) ||
	      (cmp > 0 && subset st1 cont2)

    let rec filter f st = 
      match st with
	  [] -> []
	| _ -> 
	    let lvl,elt,cont = split st in
	    let tcont = filter f cont in
	      if f elt then
		cons lvl elt tcont
	      else
		tcont

    let rec partition f st =
      match st with
	  [] -> [],[]
	| _ -> 
	    let lvl,elt,cont = split st in
	    let tcont,fcont = partition f cont in
	      if f elt then
		cons lvl elt tcont,fcont 
	      else
		tcont,cons lvl elt fcont
  end

  let union s1 s2 = Stack.zip (Stack.union (Stack.unzip s1) (Stack.unzip s2))

  let inter s1 s2 = Stack.zip (Stack.inter (Stack.unzip s1) (Stack.unzip s2))

  let diff s1 s2 = Stack.zip (Stack.diff (Stack.unzip s1) (Stack.unzip s2))

  let compare s1 s2 = Stack.compare (Stack.unzip s1) (Stack.unzip s2)

  let equal s1 s2 = Stack.equal (Stack.unzip s1) (Stack.unzip s2)

  let subset s1 s2 = Stack.subset (Stack.unzip s1) (Stack.unzip s2)

  let rec iter f = function
      Bottom -> ()
    | Level(_,down,right) -> iter f down; iter_skip f right
  and iter_skip f = function
      Node (elt,down,right) -> 
	f elt; iter f down; iter_skip f right
    | Last (elt,down) -> 
	f elt; iter f down
	  
  let rec fold f set accu = 
    match set with
	Bottom -> accu
      | Level (_,down,right) ->
	  fold_skip f right (fold f down accu)
  and fold_skip f list accu =
    match list with
	Node (elt,down,right) -> 
	  fold_skip f right (fold f down (f elt accu))
      | Last (elt,down) -> 
	  fold f down (f elt accu)
	  
  let rec for_all f = function
      Bottom -> true
    | Level (_,down,right) ->
	for_all f down && for_all_skip f right
  and for_all_skip f = function
      Node (elt,down,right) -> 
	f elt && for_all f down && for_all_skip f right
    | Last (elt,down) -> 
	f elt && for_all f down

  let rec exists f = function
      Bottom -> false
    | Level (_,down,right) ->
	exists f down || exists_skip f right
  and exists_skip f = function
      Node (elt,down,right) -> 
	f elt || exists f down || exists_skip f right
    | Last (elt,down) -> 
	f elt || exists f down

  let filter f s = 
    let tst=Stack.filter f (Stack.unzip s) in
      Stack.zip tst

  let partition f s = 
    let tst,fst=Stack.partition f (Stack.unzip s) in
      Stack.zip tst, Stack.zip fst 

  let cardinal s = fold (fun _ n->n+1) s 0

  let elements s = fold (fun x f l-> f (x::l)) s (fun x -> x) []

  let rec max_elt = function
      Bottom -> raise Not_found
    | Level (lvl,_,right) -> max_elt_skip right
  and max_elt_skip = function
      Node (_,_,right) -> max_elt_skip right
    | Last (elt,down) -> 
	try max_elt down 
	with Not_found -> elt

  let rec min_elt = function
      Bottom -> raise Not_found
    | Level (lvl,down,right) -> 
	try min_elt down 
	with Not_found -> min_elt_skip right
  and min_elt_skip = function
      Node (elt,_,_) | Last(elt,_) -> elt

  let choose = max_elt

  let print print_elt = function
      Bottom -> Format.printf "<empty>"
    | (Level (max_lvl,_,_) as set ) -> 
	let cur_width = ref 0 in
	let tab=Array.make (max_lvl + 1) "" in
	let fill i str = 
	  while String.length tab.(i) < min 75 !cur_width do
	    tab.(i) <- tab.(i)^str
	  done in
	let append i str =
	  fill i " ";
	  if String.length tab.(i) < 75 then tab.(i) <- tab.(i)^str  in
	let rec paux plvl = function
	    Bottom -> incr cur_width;incr cur_width
	  | Level(lvl,down,right) ->
	      for i=lvl+1 to plvl do 
		append i "|"
	      done;
	      append lvl "\\";
	      let dw= !cur_width + 1 in
		paux (lvl-1) down;
		paux_skip lvl right
	and paux_skip plvl = function
	    Node(elt,down,right) ->
	      fill plvl "-";
	      let str=print_elt elt in
		append plvl str;
		cur_width:= ! cur_width + String.length str-1;
		paux (plvl-1) down;
		paux_skip plvl right
	  | Last(elt,down) ->
	      fill plvl "-";
	      let str=print_elt elt in
		append plvl str;
		cur_width:= ! cur_width + String.length str-1;
		paux (plvl-1) down in
	  paux max_lvl set;
	  Format.printf "@\n";
	  for i = max_lvl downto 0 do
	    Format.printf "%s@\n" tab.(i)
	  done
	  
end

(* 

   #load "skiplist.cmo";;
   module M=struct type t=int let compare=compare end;;
   module IS=Skiplist.Make(M);; 
   let pr=IS.print string_of_int;;
   #install_printer pr;;
   open IS;;


*)

