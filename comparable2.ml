
module type COMPARABLE_2 =
sig
    type elt

    type comparison =
	|Equal
	|Lt
	|Gt

    val compare : elt -> elt -> comparison

    val random_list : int -> int -> int -> elt list

    val elt_to_value : elt -> float

    val elt_to_value_list: elt list -> float list

end
