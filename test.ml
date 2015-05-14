open Mysort
open Heap
open MinSpanningTree
open Comparable
open Comparable2
open Mysort2C

module IntComparable : COMPARABLE =
struct
 	type elt = int

   	let compare e1 e2 : int =
		if e1 > e2 then 1 else if e1 = e2 then 0 else -1;;

    	let less_than e1 e2 =
		if compare e1 e2 = -1 then true else false;;

     	let greater_than e1 e2 =
		if compare e1 e2 = 1 then true else false;;

    	let equals e1 e2 =
		if compare e1 e2 = 0 then true else false;;

	let random_list (n:int) (low:int) (high:int) =
		 Helpers.gen_int_list n low high;;

	let elt_to_value e = float_of_int e;;

	let elt_to_value_list eltli = 
	let rec _elt_to_int el li =
		match el with
		|[] -> li
		|h::t -> _elt_to_int t ((float_of_int h)::li)
		in _elt_to_int eltli [];;
end

module IntComparable2 : COMPARABLE_2 =
struct
    type elt = int

    type comparison =
	|Equal
	|Lt
	|Gt

    let compare a b =
	if a > b then Lt else if b > a then Gt else Equal;;

    let random_list (n:int) (low:int) (high:int) =
		 Helpers.gen_int_list n low high;;

    let elt_to_value e = float_of_int e;;

    let elt_to_value_list eltli = 
	let rec _elt_to_int el li =
		match el with
		|[] -> li
		|h::t -> _elt_to_int t ((float_of_int h)::li)
		in _elt_to_int eltli [];;

end

module Primms_MH = Primms_MST(Min_Heap);;
module Primms_SPTR = Primms_MST(Splay_Tree);;


module IntMergeSortBetter = Merge_Sort_Better(IntComparable);;
module IntMergeSort = Merge_Sort (IntComparable);;

module IntMergeSortTC = Merge_Sort_2C(IntComparable2);;
module IntMergeSortBetterTC = Merge_Sort_Better_2C(IntComparable2);;

module IntQuickSort = Quick_Sort (IntComparable);;
module IntQuickSortBetter = Quick_Sort_Better (IntComparable);;
module IntSplayTree = Splay_Tree(IntComparable);;
module IntMinHeap = Min_Heap(IntComparable);;


let test = IntComparable.random_list 100000 0 10000;;
let testTC = IntComparable2.random_list 100000 0 10000

let make_sorted_list l h  =
let rec _make_sorted_list ls hp =
	match IntMinHeap.get_min hp with
	|None -> (List.rev ls), hp
	|Some (el,h) -> _make_sorted_list (el::ls) h
in _make_sorted_list l h;;

let make_sorted_list_s l h =
let rec _make_sorted_list ls hp =
	match IntSplayTree.get_min hp with
	|None -> (List.rev ls), hp
	|Some (el,h) ->_make_sorted_list (el::ls) h
in _make_sorted_list l h;;


let rec add_nums l hp =
match l with
|[] ->hp
|h::t -> let hp = IntMinHeap.insert_element hp h in add_nums t hp;;

let rec add_nums_s l hp =
match l with
[] -> hp
|h::t -> let hp = IntSplayTree.insert_element hp h in add_nums_s t hp;;

(* Min Heap Test *)
let min_in_start = Sys.time ();;
let hp = add_nums test IntMinHeap.empty;;
let min_in_stop = Sys.time ();;
let min_out_start = Sys.time ();;
let sorted, hp = make_sorted_list [] hp;;
let sorted = List.rev sorted;;
let min_out_stop = Sys.time()

(* Splay Tree Test *)
let splay_in_start = Sys.time ();;
let hp_s = add_nums_s test IntSplayTree.empty;;
let splay_in_stop = Sys.time ();;
let splay_out_start = Sys.time ();;
let sorted_s, hp = make_sorted_list_s [] hp_s
let sorted_s = List.rev sorted_s
let splay_out_stop = Sys.time ();;

(*TEST SORTS*)

(* Merge Sort Test *)
let ms_s= Sys.time ();;
let sorted_ms_b = IntMergeSort.sort test;;
let ms_e = Sys.time ();;

(* Merge Sort Better Test *)
let msb_s = Sys.time ();;
let sorted_ms = IntMergeSortBetter.sort test;;
let msb_e = Sys.time ();;

(* Merge Sort type compare Test *)
let mstc_s = Sys.time();;
let sorted_mstc = IntMergeSortTC.sort testTC;;
let mstc_e = Sys.time();;

(* Merge Sort Better type compare Test*)
let msbtc_s = Sys.time();;
let sorted_msbtc = IntMergeSortBetterTC. sort testTC;;
let msbtc_e = Sys.time();;

(* Quick Sort Test *)
let qs_s = Sys.time ();;
let sortedQ = IntQuickSort.sort test;;
let qs_e = Sys.time ();;

(* Quick Sort Better Test *) (*
let qsb_s = Sys.time ();;
let sorted_qsb = IntQuickSortBetter.sort test;;
let qsb_e = Sys.time ();;
*)

(* Stock Ocaml Sort Test *)
let stk1_s = Sys.time ();;
let sorted_stock_1 = List.sort (IntComparable.compare) test;;
let stk1_e = Sys.time ();;


(* Stock Ocaml Sort 2 Test *)

let _ = Printf.printf "list count- %d\n" (List.length sorted)
let _ = Printf.printf "Min_Heap insert time %f\n" (min_in_stop -. min_in_start);;
let _ = Printf.printf "Min_Heap remove time %f\n" (min_out_stop -. min_out_start);;
let _ = Printf.printf "Splay_Tree insert time %f\n" (splay_in_stop -. splay_in_start);;
let _ = Printf.printf "Splay_Tree remove time %f\n\n" (splay_out_stop -. splay_out_start)

(*
let _ = (Printf.printf "Quick Sort time start- %f\n" qs);;
let _= (Printf.printf "Quick Sort time stop- %f\n"  qe);;
let _ = (Printf.printf "Merge Sort time start- %f\n" ms);;
let _ = (Printf.printf "Merge Sort time stop- %f\n" me);;
let _ = (Printf.printf "Merge Sort Better time start- %f\n" msb);;
let _ = (Printf.printf "Merge Sort Better time stop- %f\n" meb);;
*)

let _ = Printf.printf "Min_Heap sort time %f\n" ((min_in_stop -. min_in_start) +. (min_out_stop -. min_out_start))
let _ = Printf.printf "Splay Tree sort time %f\n\n" ((splay_in_stop -. splay_in_start) +. (splay_out_stop -. splay_out_start))

let _ = (Printf.printf "Merge Sort sort time- %f\n" (ms_e -. ms_s));;
let _ = (Printf.printf "Merge Sort Better sort time - %f\n\n" (msb_e -. msb_s));;
let _ = (Printf.printf "Merge Sort type compare sort time %f\n" (mstc_e -. mstc_s));;
let _ = (Printf.printf "Merge Sort Better type compare sort time %f\n\n" (msbtc_e -. msbtc_s));;
let _ = (Printf.printf "Quick Sort sort time- %f\n" (qs_e -. qs_s));;
(*let _ = Printf.printf "Quick Sort Better sort time- %f\n\n" (qsb_e -. qsb_s);;
*)
let _ = Printf.printf "Ocaml Library Merge Sort time - %f\n" (stk1_e -. stk1_s);;


(*let _ = Helpers.print_float_array (IntComparable.elt_to_value_list sorted);;*)

assert((sorted) = (sortedQ));;
assert((sorted_ms) = (sorted_ms_b));;




(*
let () = Helpers.print_int_array (IntComparable.elt_to_value_list sorted);;
*)


