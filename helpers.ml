

let rec print_int_array (l:int list):unit =
	match l with
	|[] -> Printf.printf "\n"; ()
	|h::t -> (Printf.printf "%1d" h; Printf.printf ";"; print_int_array t);;

let rec print_float_array (l:float list) =
	match l with
	|[] -> Printf.printf "\ndone\n"; ()
	|h::t -> (Printf.printf "%1f" h; Printf.printf ";"; print_float_array t);;

let gen_int_list n low high =
	Random.self_init ();
	let rec gen_random n ls =
		if n > 0 
then let i = ((Random.int (high - low)) + low) in
				gen_random (n - 1) (i::ls)
			else ls
	in gen_random n [];;

let gen_float low high : float =
	Random.self_init ();
	Random.float ((float_of_int high) -. (float_of_int low)) +. (float_of_int low);;

let gen_int low high : int =
	Random.self_init ();
	Random.int (high - low) + low;;
