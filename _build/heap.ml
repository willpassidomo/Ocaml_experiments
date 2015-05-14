open Comparable


module type HEAP = functor (C:COMPARABLE) ->
sig
	type elt = C.elt
	type heap
    
    val empty : heap
    
    val insert_element : heap -> elt -> heap
    
    val is_empty : heap -> bool
    
    val get_min : heap -> (elt * heap) option
    
end

(* chose a max heap over a min_heap, because max heap has the same
number of operations / O() as min_heap, but is tail recursive in returning
the updated heap *)
(*
module Min_Heap (C:COMPARABLE) : (HEAP with type elt = C.elt) =
struct
	type elt = C.elt
	type heap =
		|Nul
		|Cons of heap * int * elt * heap * int
				(* ints are number of children *)

	let empty = Nul;;

	let is_empty h = 
		match h with
		|Nul -> true
		|Cons (_,_,_,_,_) -> false;;

	let insert_element (h:heap) (e:elt) =
	let rec _insert_element hp el =
		match hp with
		|Nul -> Cons(Nul,0,el,Nul,0)
		|Cons (l,ln,el1,r, rn) -> 
			if rn < ln 
				then 
					(if C.less_than el el1
						then Cons(l,ln,el,_insert_element r el1, (rn + 1))					
						else Cons(l,ln,el1,_insert_element r el, (rn + 1)))
				else 
					if C.less_than el el1
						then Cons(_insert_element l el1, (ln + 1),el,r,rn)					
						else Cons(_insert_element l el, (ln + 1),el1,r,rn)
		in _insert_element h e;;

	let get_min (hp:heap)  =
	let rec replace_min h =
		match h with
		|Nul	-> Nul
		|Cons(l,ln,min,r,rn) ->		
			match l,r with
			|Nul, Nul -> Nul
			|Nul, Cons (_,_,el,_,_) -> Cons (l,ln,el,replace_min r,(rn - 1))
			|Cons (_,_,el,_,_), Nul -> Cons (replace_min l, (ln -1),el,r,rn)
			|Cons(_,_,ell,_,rn), Cons(_,_,elr,_,_) -> 
				(if C.less_than ell elr
					then Cons(replace_min l, (ln - 1), ell,r,rn)
					else Cons(r,rn,elr,replace_min r, (rn - 1)))
	in match hp with
		|Nul	-> None
		|Cons(l,_,min,r,_) -> let h = replace_min hp in Some(min, h);;
(*
	let count (h:heap) : int = 
		match h with
		|Nul -> 0
		|Cons (_,l,_,_,r) -> (l + r);;
*)

	let count (h:heap) : int =
	let rec _count hp  : int =
		match hp with 
		|Nul -> 1
		|Cons (l,_,_,r,_) -> (_count l ) + (_count r)
	in _count h ;;

	let member e h = true;;

end
*)

module Splay_Tree = functor (C:COMPARABLE) ->
struct
	type elt = C.elt
	type tree = 
		|Leaf
		|Node of tree * elt list * tree
	type heap = tree ref

	let empty = ref Leaf;;

	let head (e : elt list) : elt =
		match e with
		|[] -> failwith "Impossible, no element"
		|h::_ -> h;;

	let is_empty (h:heap) =
		match !h with
		|Leaf -> true
		|Node(_,_,_) -> false;;
	
	let rec splay (e:elt) (t:tree) : tree = 
		match t with
		|Leaf -> Leaf
		|Node (l1, e1, r1) ->
			if C.equals e (head e1) 
				then Node(l1,e1, r1) else
			if C.less_than e (head e1)
				then 
				(match l1 with
				|Leaf -> t
				|Node(l2, e2, r2) ->
					(if C.equals e (head e2) 
						then Node(l2, e2, Node(r2 , e1, r1)) else
					if C.less_than e (head e2)
						then
						(match l2 with
						|Leaf -> Node (l2, e2, Node(r2, e1, r1))
						|Node(_,_,_) ->
							let Node (ll, ee, rr) = splay e l2 in
							Node (ll, ee, Node (rr, e2, Node(r2, e1, r1)))) else
					if C.greater_than e (head e2)
						then
						(match r2 with 
						|Leaf -> Node(l2, e2, Node (r2, e1, r1))
						|Node (_,_,_) -> 
							let Node (ll, ee, rr) = splay e r2 in
							Node (Node (l2, e2, ll), ee, Node (rr, e1, r1))) else
						failwith "Impossible")) else
			if C.greater_than e (head e1)
				then 
				(match r1 with
				|Leaf -> t
				|Node (l2, e2, r2) ->
					(if C.equals e (head e2)
						then Node (Node(l1, e1, l2),e2, r2) else
					if C.less_than e (head e2)
						then
						(match l2 with
						|Leaf -> Node(Node(l1,e1,l2),e2,r2)
						|Node(_,_,_) -> 
							let Node (ll,ee,rr) = splay e l2 in
							Node(Node(l1,e1,ll),ee, Node (rr,e2,r2))) else
					if C.greater_than e (head e2)
						then 
						(match r2 with
						|Leaf -> Node(Node(l1,e1,l2), e2, r2)
						|Node (_,_,_) -> 
							let Node (ll,ee,rr) = splay e r2 in
Node(Node(Node(l1,e1,l2),e2,ll),ee,rr))else
						failwith "Impossible")) else 
			failwith "Impossible";;

	let member (h:heap) (e:elt) : bool =
		match !h with
			|Leaf -> false
			|Node(_,_,_) ->
				h := splay e !h;
				match !h with
				|Leaf -> failwith "Impossible"
				|Node(_,e1,_) -> 
					if C.equals e (head e1)
						then true else false;;

	let insert_element (h:heap) (e:elt) : heap =
		match !h with
		|Leaf -> ref (Node(Leaf,([e]),Leaf))
		|Node(_,_,_) ->
			let Node(l,e1,r) = splay e !h in
			if C.greater_than e (head e1)
				then ref (Node(Node(l,e1,Leaf), [e],r)) else
			if C.less_than e (head e1) 
				then ref (Node(l,[e], Node(Leaf,e1,r))) else
			if C.equals e (head e1)
				then ref (Node(l,(e::e1),r)) else
				failwith "Impossible";;

	let ls_empty (e:elt list) : bool =
		match e with
		|[] -> true
		|_::_ -> false;;

	let delete (e:elt) (h:heap) : heap =
		match !h with
		|Leaf -> ref Leaf
		|Node(_,_,_) -> 
			let Node (l,v,r) = splay e (!h) in
			if C.equals e (head v)
				then
				(match v with
				|[] -> failwith "Impossible"
				|[a] ->
					(match l,r with
					|Leaf, _ -> ref r
					|_,Leaf -> ref l
					|Node(_,_,_), Node(_,_,_) ->
						let Node(ll,vv,rr) = splay e l
						in ref (Node(ll,vv,r)))
				|hd::t ->
					ref (Node(l,t,r)))
			else ref(Node(l,v,r));;

	let get_min (h:heap) : (elt*heap)option =
	let rec _get_min (t:tree) =
		match t with
		|Leaf -> None
		|Node(l,v,r) ->
			(match l, r with
			|Leaf , _ -> Some(head v)
			| _,_ -> _get_min l) in
		let min = _get_min !h in
		match min with
		|None -> None
		|Some(m) -> 
			let hp = delete m h in
			Some(m,hp);;

	let max a b =
		if a > b then a else b;;

	let depth (h:heap) : int =
	let rec _depth (t:tree) =
		match !h with
		|Leaf -> 0
		|Node (l,_,r) -> max (_depth l) (_depth r) + 1
	in _depth !h;;
end


module Min_Heap = functor(C:COMPARABLE) ->
struct
  type elt = C.elt
  type tree = 
	|Leaf 
	|Node of tree * elt * tree
  type heap = Heap of int * tree

  let empty = Heap (0, Leaf)

  let is_empty (h:heap) =
	let Heap(_,t) = h in
	match t with
	|Leaf -> true
	|Node (_,_,_) -> false

  let rec ins (n:int) (x:elt) (t:tree) =
    match t with
	Leaf -> Node (Leaf, x, Leaf)
      | Node (l, y, r) ->
	  if n mod 2 = 0
	  then 
	    let (Node (ll, ee, rr) as tt) = ins (n/2) x l
	    in 
	      if ee < y
	      then  Node (Node (ll, y, rr), ee, r)
	      else Node (tt, y, r)
	  else 
	    let (Node (ll, ee, rr) as tt) = ins (n/2) x r
	    in 
	      if ee < y
	      then  Node (l, ee, Node (ll, y, rr))
	      else Node (l, y, tt)

  let insert_element (h:heap) (e:elt) =
	let Heap (n, t) = h in 
	Heap (n + 1, ins (n+1) e t)

  let find_min (Heap (_, t)) =
    match t with
	Node (_, x, _) -> Some(x)
      | Leaf -> None
  let rec check (Node (l, x, r) as t) =
    match (l,r) with
	(Leaf,Leaf) -> t
      | (Leaf, Node (rl, xr, rr)) ->
	  if x > xr
	  then Node (l, xr, Node (rl, x, rr))
	  else t
      | (Node (ll, xl, lr), Leaf) -> 
	  if x > xl
	  then Node (Node (ll, x, lr), xl, r)
	  else t
      | (Node (ll, xl, lr), Node (rl, xr, rr)) ->
	  if x <= xl && x <= xr
	  then t
	  else if xl < xr
	  then Node (check (Node (ll, x, lr)), xl, r)
	  else Node (l, xr, check (Node (rl, x, rr)))

  let rec del (n, Node (l, x, r)) =
    match (l, r) with
	(Leaf, Leaf) -> (x, Leaf)
      | _ ->
	  if n mod 2 = 0
	  then 
	    let (ee, newL) = del (n/2, l)
	    in (x, check (Node (newL, ee, r)))
	  else
	    let (ee, rr) = del (n/2, r)
	    in (x, check (Node (l, ee, rr)))

  let delete_min (Heap(n, t)) =
    match t with
	Leaf -> Heap (0, Leaf)
      | _ -> 
	  let (ee, tt) = del (n, t)
	  in Heap (n-1, tt)
 
   let get_min (h:heap) =
	match find_min h with
	|None -> None
	|Some(a) -> Some (a, (delete_min h))

end
(*
module Fibonicci_Heap : HEAP =
   struct 
	type verticie = int
	type edge = verticie * float * verticie
    	type key = float
	type value = edge
	type elt = (key * value)
	type heap = int * node
			(* ((min * node)) *)
	type node =
       		|Null 
        	|Node of heap * heap * bool * int * key * value * heap * heap
			(* ((Before * After * marked * rank key * value * Child * Parent)) *)

    
    	let empty = Leaf
    
    	let is_empty (t:heap) =
        	match t with
        	|Leaf-> true
        	|Node(_,_,_,_,_,_,_) -> false;;

	let make_child (par:node) (child:node) : node =
		match a , b with
		|Null, Null || Null, Node(a) || Node(a) , Null -> raise Exception
		|Node (ba, aa, ma, ra, va, ca,pa), Node (bb,ab,mb,rb,vb,cb,pb) ->
			Node (ba, aa, ma, ra, va, Node(bb,ab,mb,rb,vb,cb,

	let link_trees (a:node) (b:node) : node =
		match a , b with
		|Null, Null -> Null
		|Null, Node (a) || Node (a), Null -> Node (a)
		|Node (_,_,_,ra,_,_,_), Node (_,_,_,rb,__,_) ->
			if ra < rb
				then make_child a b
				else make_child b a

	let insert_element (h:heap) (k:key) (v:value) : heap =
		let (min, n) = h in
		if k < min then min = k 
		match n with
		|Null -> 
		|Node (bef,aft,marked,rank,kh,_,child,parent) -> 
			if k < kh 
				then Node(Null, h, false, rant + 1, k
	
	let peek (h:heap) = 
		let (min,_) = h in
		min;;


	let get_mun (h:heap)= None
	let sort (h:heap) = empty

end
*)

(*
module Min_Heap_Sequencer(H:MIN_HEAP) : (SEQUENCER with type elt = H.elt and type sequencer = H.heap) =
struct
    type elt = H.elt;;
    type sequencer = H.heap;;
    
    let empty = H.empty;;
    let is_empty (l:sequencer) = H.is_empty l;;
    let get_min (l:sequencer) = H.get_min l;;
    let insert_element = H.insert_element;;  
    let finish_insert (l:sequencer) = l;;
end
*)
(*
module Binary_Heap : MIN_HEAP =
(* raise Todo *)
end
*)
