open Comparable

module type SORT = functor (C:COMPARABLE) ->
sig
    type elt = C.elt
    
    val empty : elt list
    
    val insert_element : elt list -> elt -> elt list
    
    val is_empty : elt list -> bool
    
    val remove_min : elt list -> (elt * elt list) option
    
    val sort : elt list -> elt list

   val run_test : unit -> unit

end



module Merge_Sort = functor (C:COMPARABLE) ->
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
				if C.greater_than hr hl 
					then _merge l tr (hr::sorted)
					else _merge tl r (hl::sorted))
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



module Merge_Sort_Better = functor (C:COMPARABLE) ->

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
			(if C.greater_than a hf
				then (Fwd(a::[])::co::o)
				else (Rev(a::hf::tf)::o))
		|Fwd (hf::tf) ->	
			(if C.less_than a hf
				then (Rev(a::[])::co::o)
				else (Fwd(a::hf::tf)::o)))
	|h::m::t -> 
		match co with
		|Rev (hf::tf) -> 
			if C.greater_than h hf 
				then 
				(if C.less_than m h 
					then _split_list t (Rev(m::h::[])) (co::o)
					else _split_list t (Fwd(m::h::[])) (co::o))
				else _split_list (m::t) (Rev(h::hf::tf)) o
		|Fwd (hf::tf) ->
			(if C.less_than h hf
				then
				(if C.less_than m h
					then _split_list t (Rev(m::h::[])) (co::o)
					else _split_list t (Fwd(m::h::[])) (co::o))
				else _split_list (m::t) (Fwd(h::hf::tf)) o) in
	match el with
	|[] -> []
	|[a] -> (Fwd(a::[])::[])
	|h::m::t ->
		if C.less_than m h
			then _split_list t (Rev(m::h::[])) []
			else _split_list t (Fwd(m::h::[])) [];;
	
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
			(if C.greater_than ha hb
				then _merge (Fwd(ta)) r (Rev(ha::a))
				else _merge l (Fwd(tb)) (Rev(hb::a)))
		| (Fwd ([]),Fwd(h::t)) | (Fwd(h::t), Fwd([])) -> _merge (Fwd(t)) (Fwd([])) (Rev(h::a))
		| Fwd ([]), Fwd([]) -> sorted
		| _ ,_ -> failwith "wrong order list")
	|Fwd(a) ->
		match l, r with
		|Rev(ha::ta), Rev (hb::tb) -> 
			(if C.less_than ha hb
				then _merge (Rev(ta)) r (Fwd(ha::a))
				else _merge l (Rev(tb)) (Fwd(hb::a)))
		|Rev ([]), Rev(h::t) | Rev(h::t), Rev([]) -> _merge (Rev(t)) (Rev([])) (Fwd(h::a))
		|Rev([]), Rev([]) -> sorted
		|_,_ -> failwith "wrong order list";;


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
				|[],[_] -> unsorted u_r s_f [] []
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

module Quick_Sort  = functor (C:COMPARABLE) ->
struct
    type elt = C.elt;;
    
    let empty : elt list = [];;
    
    let is_empty (l:elt list) =
        match l with
        |[] -> true
        |_::_-> false
    
    let insert_element (l:elt list) (e:elt) : elt list = e::l;;
    
    let remove_min (l:elt list) : (elt * elt list) option = 
	match l with
	|[] -> None
	|h::t -> Some (h,t);;
    
								(* false, true)= *)
    let partition (f: 'a -> bool) (l:elt list) : (elt list * elt list) =    
	let rec _partition (ls:elt list) (fls:elt list) (tru:elt list) = 
	match ls with 
	|[] -> (fls, tru)
	|h::t -> if f h 
			then _partition t (h::fls) tru 
			else _partition t fls (h::tru)
	in _partition l [] [];;
    
    let sort (unsorted:elt list) : elt list =
    let rec _sort (u:elt list) =
	match u with
	|[] -> []
	|h::t -> 
let (left,right) = partition (C.less_than h) t  in 
   	_sort left @ h :: _sort right
    in _sort unsorted;;

    let run_test _ = ()
end


module Quick_Sort_Better = functor (C:COMPARABLE) ->
struct
    type elt = C.elt;;
    
    let empty : elt list = [];;
    
    let is_empty (l:elt list) =
        match l with
        |[] -> true
        |_::_-> false
    
    let insert_element (l:elt list) (e:elt) : elt list = e::l;;
    
    let remove_min (l:elt list) : (elt * elt list) option = 
	match l with
	|[] -> None
	|h::t -> Some (h,t);;
    
								(* false, true)= *)
    let partition (f: 'a -> bool) (l:elt list) : (elt list * elt list) =    
	let rec _partition (ls:elt list) (fls:elt list) (tru:elt list) = 
	match ls with 
	|[] -> (fls, tru)
	|h::t -> if f h 
			then _partition t (h::fls) tru 
			else _partition t fls (h::tru)
	in _partition l [] [];;
    
    let random_el (l:elt list) : (elt * elt list) option =
	Random.self_init ();	
	let ll = List.length l in
	let index = Random.int (ll + 1) in
	let rec get_index n li lu =
		if n > 0 
			then
				(match li with
				|[] -> get_index (n-1) lu li
				|h::t -> get_index (n-1) t (h::lu))
			else
				match li with
				|[] -> None
				|h::t -> 
					if (index * 2) > ll 
						then Some(h, t@lu)
						else Some(h, lu@t)
	in get_index index l []


    let sort (unsorted:elt list) : elt list =
	let llength = List.length unsorted in
	let rec _sort (u:elt list) =
		match random_el u with
		|None -> []
		|Some(h,t) ->		
			let (left,right) = partition (C.less_than h) t  in 
   			_sort left @ h :: _sort right
   	in _sort unsorted;;

    let run_test _ = ()
end


