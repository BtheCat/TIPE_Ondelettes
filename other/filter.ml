#use "array.ml";;

let makeOnFilter type_filter param_filter =
	match (type_filter, param_filter) with
		| ("Haar", _) -> 
