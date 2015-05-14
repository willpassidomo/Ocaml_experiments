open Heap
open Comparable


module type MIN_SPANNING_TREE =
   sig
	type verticie = int
	type edge = verticie * float * verticie
	type mst


	val do_mst : edge list -> mst
	
	val print_mst : mst -> unit


end

(*module Edge_Comparable : COMPARABLE =
struct
	type verticie = int
	type elt = verticie * float * verticie

	let compare a b =
		let (_,va,_),(_,vb,_) = a, b in
		if va > vb then 1 else
			if vb > va then -1 else 0;;
	let greater_than a b = if compare a b = 1 then true else false;;
	let less_than a b = if compare a b = -1 then true else false;;
	let equals a b = if compare a b = 0 then true else false;;
	let random_list n low high =
	let rec gen_random ne ls =
		gen_random (ne - 1) ((Helpers.gen_int low high, 					Helpers.gen_float low high, Helpers.gen_int low high)::ls)
	in gen_random n [];;
	let elt_to_value e : float =
		let (_,v,_) = e in v;;
	let elt_to_value_list (e:elt list) : float list =
	let rec build_list el ls =
		match el with
		|[] -> ls
		|h::t -> build_list t ((elt_to_value h)::ls)
	in build_list e [];;
end
*)
(*

module type EDGE_HEAP =
sig 
	type verticie = int
	type edge = verticie * float * verticie
	type elt = Edge_Comparable.elt
	type heap
	val empty : heap
        val insert_element : heap -> elt -> heap
   	val is_empty : heap -> bool
     	val get_min : heap -> (elt * heap) option
end 


module Min_Heap_Edges : EDGE_HEAP =
struct
	type verticie = int
	type edge = verticie * float * verticie
	module M= Min_Heap(Edge_Comparable);;


	type heap = M.heap
	let empty = M.empty;;
	let insert_element h e = M.insert_element h e;;
	let is_empty h = M.is_empty h;;
	let get_min h = M.get_min h;;
end

*)

module Primms_MST (H:HEAP) : MIN_SPANNING_TREE =
struct 
	exception UnconnectedTree

	type verticie = int
	type edge = verticie * float * verticie
	type mst = edge list option

module Edge_Comparable: (COMPARABLE with type elt = edge) = 
	(struct
	
	type elt = edge

	let compare a b =
		let (_,va,_),(_,vb,_) = a, b in
		if va > vb then 1 else
			if vb > va then -1 else 0;;
	let greater_than a b = if compare a b = 1 then true else false;;
	let less_than a b = if compare a b = -1 then true else false;;
	let equals a b = if compare a b = 0 then true else false;;
	let random_list n low high =
	let rec gen_random ne ls =
		gen_random (ne - 1) ((Helpers.gen_int low high, 			Helpers.gen_float low high, Helpers.gen_int low high)::ls)
	in gen_random n [];;
	let elt_to_value e : float =
		let (_,v,_) = e in v;;
	let elt_to_value_list (e:elt list) : float list =
	let rec build_list el ls =
		match el with
		|[] -> ls
		|h::t -> build_list t ((elt_to_value h)::ls)
	in build_list e [];;
end);;

	module M = H(Edge_Comparable);;

	type heap = M.heap

	(* removes the edge with the smallest distance and returns remaining edge list and min *)
	let get_starting_edge (edges : edge list) : int option =
	let rec get_min (unsearched : edge list) (searched : edge list) (min : edge) =
let (_,min_d,_) = min in
		match unsearched with
		|[] -> let (p,_,_) = min in Some(p)
		|h::t -> 
			let (_,d,_) = h in
			if d < min_d 
				then get_min t (min::searched) h 
				else get_min t (h::searched) min
	in 
	match edges with 
	|[] -> None
	|h::t -> get_min t [] h;;

	let rec insert_elements (m : M.heap) (edges:edge list)  =
		match edges with
		|[] -> m
		|h::t -> let m = M.insert_element m h in
				insert_elements m t

	(* removes all edges which are neighbors of target edge "e", returns (neighbor edges * remaining edges) *)
	let get_neighbors (p : int) (edges : edge list) : (edge list * edge list) =
	let rec find_point (p:int) (unsearched:edge list) (searched:edge list) (neighbors:edge list) =
		match unsearched with
		|[] -> (searched, neighbors)
		|h::t -> let (p1,_,_) = h in 
				if p = p1 then find_point p t searched (h::neighbors)
					else find_point p t (h::searched) neighbors
	in find_point p edges [] [];;


	let get_mst (edges:edge list) (starting : int): edge list option  = 
		(* get all neighbors of edge *)
		let (neighbors, edges) = get_neighbors starting edges in
		let m = insert_elements M.empty neighbors in
		(* follow the path of least resistance untill strongly connected component is exhausted (return None), or edges are exhausted (return MST) *)		
		let rec find_tree (edges:edge list) (mst:edge list) (m:M.heap)  = 			match M.get_min m with
					|None -> (match edges with
				(* no more edges left, done *)
							|[] -> Some(mst)
				(* not sure if this is right, need to check if POINTS are left, but when a point is connected all it's neighbors are put on the stack, so edges should be empty *)
							|h::t -> None)
					|Some (edge_min, m) ->
				(* run again with neighbors added to min heap and min edge added to mst *)
					let (p,_,_) = edge_min in				
					let (neighbors,edges) = get_neighbors p edges 	
		in find_tree edges (edge_min::mst) (insert_elements m neighbors)
		in find_tree edges [] m;;
		

	let do_mst (edges : edge list) : mst =
		(* get starting edge, min distance *)
		match get_starting_edge edges with
		|None -> None
		(* find mst *)
		|Some (p) ->get_mst edges p ;;
		
	let print_mst (min_tree : mst) = () (* TODO *)

end


(*
module Kruskles (S:SORT) (T:TreeSet) : MIN_SPANNING_TREE =
   struct 
	set = T.set
	elt = S.elt
	lst = S.lst

	type edge = verticie * float * verticie
	type mst = edge list option
	
	let sort_edges (e : edge list) =
	let rec _sort_edges (edges:edge list) (l:lst) =
		match edges with
		|[] -> S.sort l
		|h::t -> S.insert_element h; _sort_edges t
	in _sort_edges e S.empty;;
	
	let make_sets (e : edge list) =
	let rec _make_sets (edges: edge list) (s:set list) =		
		match edges with
		|[] -> s
		|h::t -> _make_sets t ((T.singleton h)::s)
	in _make_sets e [];;

	let union_sets (s : set list) : set =
	let rec _union_sets (sets:set list) (s:set) =
		match sets with
		|[] -> s
		|h::t -> _union_sets t (S.union h s)
	in _union_sets s S.empty;;

	let do_mst (edges : edge list) : mst =
		let edges = sort_edges edges in
		let sets = make_sets edges in
		let s = union_sets sets in
		S.make_list s;;

	let print_mst (m : mst) : unit = ();;
	
end	

*)

