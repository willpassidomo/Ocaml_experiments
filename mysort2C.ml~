open Comparable2

module type SORT = functor (C:COMPARABLE_2) ->
sig
    type elt = C.elt
    
    val empty : elt list
    
    val insert_element : elt list -> elt -> elt list
    
    val is_empty : elt list -> bool
    
    val remove_min : elt list -> (elt * elt list) option
    
    val sort : elt list -> elt list

   val run_test : unit -> unit

end



module Merge_Sort_2C = functor (C:COMPARABLE_2) ->
struct
    type elt = C.elt
    
    let empty : elt list = [];;
    
    let is_empty (l:elt list) = 
        match l with
        |[] -> true 
        | _::_ -> false
    let insert_element (l:elt list) (e:elt) = 
	e::l;;
    
    let remove_min (l:elt list) : (elt * elt list) option = 
	match l with
	|[] -> None
	|h::t -> Some(h , t) ;;

    let split_list (l:elt list) =
    let rec _split_list (ls:elt list) (lf:elt list) (rt:elt list) : (elt list * elt list) =
	match ls with
	|[] -> (lf, rt)
	|h::t -> _split_list t rt (h::lf)
    in _split_list l [] [];;

    let merge (left:elt list) (right:elt list) : elt list =
	let rec _merge (l:elt list) (r:elt list) (sorted:elt list) =
		match l with
		|[] -> (match r with
			|[] -> sorted
			|hr::tr -> _merge [] tr (hr::sorted))
		|hl::tl -> 
			(match r with
			|[] -> _merge tl [] (hl::sorted)
			|hr::tr -> 
				match C.compare hr hl with
				|Gt -> _merge l tr (hr::sorted)
				|Lt | Equal -> _merge tl r (hl::sorted))
	in List.rev (_merge left right []);;

    let rec sort (unsorted:elt list) : elt list = 
	match unsorted with
	|[] -> []
	|[a] -> [a]
	|_::_ -> let (left, right) = split_list unsorted in 
			merge (sort left) (sort right);;

    let run_test _ = ()
	(* TODO *)
end



module Merge_Sort_Better_2C = functor (C:COMPARABLE_2) ->

struct

    type elt = C.elt

    type order =
	|Rev of elt list
	|Fwd of elt list  
    let empty : elt list = [];;
    
    let is_empty (l:elt list) = 
        match l with
        |[] -> true 
        | _::_ -> false

    let insert_element (l:elt list) (e:elt) = 
	e::l;;
    
    let remove_min (l:elt list) : (elt * elt list) option = 
	match l with
	|[] -> None
	|h::t -> Some(h , t) ;;

    let split_list (el:elt list) : order list=
    let rec _split_list (l:elt list) (co:order) (o:order list) =
	match l with
	|[] -> (co::o)
	|[a] ->
		(match co with 
		|Rev (hf::tf) ->
			(match C.compare a hf with 
			|Gt -> (Fwd(a::[])::co::o)
			|Lt | Equal -> (Rev(a::hf::tf)::o))
		|Fwd (hf::tf) ->	
			(match C.compare a hf with
			|Lt -> (Rev(a::[])::co::o)
			|Gt | Equal -> (Fwd(a::hf::tf)::o)))
	|h::m::t -> 
		match co with
		|Rev (hf::tf) ->
			(match C.compare h hf with
			|Equal | Lt -> _split_list (m::t) (Rev(h::hf::tf)) o
			|Gt -> 
				match C.compare m h with
					|Lt -> _split_list t (Rev(m::h::[])) (co::o)
					|Equal | Gt -> _split_list t (Fwd(m::h::[])) (co::o))
				
		|Fwd (hf::tf) ->
			match C.compare h hf with
			|Equal | Gt -> _split_list (m::t) (Fwd(h::hf::tf)) o
			|Lt ->
				match C.compare m h with
				|Lt -> _split_list t (Rev(m::h::[])) (co::o)
				|Gt | Equal -> _split_list t (Fwd(m::h::[])) (co::o)
	 in
	match el with
	|[] -> []
	|[a] -> (Fwd(a::[])::[])
	|h::m::t ->
		match C.compare m h with
		|Lt -> _split_list t (Rev(m::h::[])) []
		|Gt | Equal -> _split_list t (Fwd(m::h::[])) [];;
	
    let split_order_list (ol:order list) : (order list * order list) =
	let rec _split_order_list li fwd rev =
	match li with
	[] -> (fwd, rev)
	|h::t ->
		(match h with
		|Rev(_) -> _split_order_list t fwd (h::rev)
		|Fwd(_) -> _split_order_list t (h::fwd) rev)
	in _split_order_list ol [] [];;

   let rec _merge (l:order) (r:order) (sorted:order) =
	match sorted with
	|Rev(a) ->
		(match l, r with
		|Fwd (ha::ta), Fwd(hb::tb) -> 
			(match C.compare ha hb with
			|Gt -> _merge (Fwd(ta)) r (Rev(ha::a))
			|Lt | Equal -> _merge l (Fwd(tb)) (Rev(hb::a)))
		| (Fwd ([]),Fwd(h::t)) | (Fwd(h::t), Fwd([])) -> _merge (Fwd(t)) (Fwd([])) (Rev(h::a))
		| Fwd ([]), Fwd([]) -> sorted
		| _ ,_ -> failwith "wrong order list")
	|Fwd(a) ->
		match l, r with
		|Rev(ha::ta), Rev (hb::tb) -> 
			(match C.compare ha hb with
			|Lt -> _merge (Rev(ta)) r (Fwd(ha::a))
			|Gt | Equal -> _merge l (Rev(tb)) (Fwd(hb::a)))
		|Rev ([]), Rev(h::t) | Rev(h::t), Rev([]) -> _merge (Rev(t)) (Rev([])) (Fwd(h::a))
		|Rev([]), Rev([]) -> sorted
		|_,_ -> failwith "wrong order list";;


(*
   let merge (orders:order list) : elt list =

	let rec merge_orders (ol:order list) (sl:order list) : elt list =
		match ol with
		|[] ->
			(match sl with
			|[] -> []
			|[a] -> a
			| _ -> merge_orders sl [])
		|[a] -> 
			(match sl with
			|[] -> a
			|h::t -> merge_orders (a::h::t) [])
		|h::m::t -> 
			merge_orders t ((_merge h m [])::sl)
	in merge_orders orders ([[]]);;

*)
	let print_time a b c d=
	Printf.printf "MS_B split1 time- %f\n" (a -. b);
	Printf.printf "MS_B split2 time- %f\n" (b -. c);
	Printf.printf "MS_B merge time -%f\n" (c -. d);
	();;

    let sort (el:elt list) : elt list = 
	match el with
	|[] -> []
	|[a] -> [a]
	|_::_ -> 
(*TODO remove timing *)
		let stsplt = Sys.time () in
		let orders = split_list el in
		let ensplt = Sys.time () in
		let (fwd, rev) = split_order_list orders in
		let ennneplt = Sys.time () in
		let rec unsorted u_r u_f s_r s_f : order =
			match u_r, u_f with
			|[], [] -> 
				(match s_r, s_f with
				|[], [] -> (Fwd([]))
				|[],[a] -> a
				|[a],[] -> a
				|_,_ -> unsorted s_r s_f [] [])
			|[a],[] -> 
				(match s_r, s_f with
				|[],[] -> a
				|[y],[] -> _merge a y (Fwd([]))
				|[],[z] -> unsorted u_r s_f [] []
				|_,_ -> unsorted (a::s_r) s_f [] [])
			|[],[b] ->
				(match s_r, s_f with
				|[],[] -> b
				|[_],[] -> unsorted s_r u_f [] []
				|[], [z] -> _merge z b (Rev([]))
				| _,_ -> unsorted s_r (b::s_f) [] [] )
			|[a],[b] -> 
				(match s_r,s_f with
				|[],[] ->
					(match a with
					|Fwd(_) -> failwith "wrong order"
					|Rev(aa) ->
						_merge (Fwd(List.rev aa))  b (Rev([])))
				|_,_ -> unsorted (a::s_r) (b::s_f) [] [])
			|hr::mr::tr, hf::mf::tf ->
				unsorted tr tf ((_merge hf mf (Rev([])))::s_r) ((_merge hr mr (Fwd([])))::s_f)
			|_,h::m::t -> unsorted u_r t ((_merge h m (Rev([])))::s_r) s_f 
			|h::m::t,_ -> unsorted t u_f s_r ((_merge h m (Fwd([])))::s_f)
		in match unsorted rev fwd [] [] with
			|Rev(a) -> print_time stsplt ensplt ennneplt (Sys.time ()); List.rev a
			|Fwd(a) -> print_time stsplt ensplt ennneplt (Sys.time ()); a;;

    let run_test _ = ()
	(* TODO *)
end

