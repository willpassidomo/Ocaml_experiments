
module type COMPARABLE =
sig
    type elt



    val compare : elt -> elt -> int

    val less_than : elt -> elt -> bool

    val greater_than : elt -> elt -> bool

    val equals : elt -> elt -> bool

    val random_list : int -> int -> int -> elt list

    val elt_to_value : elt -> float

    val elt_to_value_list: elt list -> float list

end
