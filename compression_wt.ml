#use "array.ml";;
#use "fwt_or_bi.ml";;

let abs x =
	if x < zero then op x
	else x

let abs2 x =
	if x < 0 then - x
	else x

let haar_filter = Vect.prod_scal (Vect.vect_of_array [|1. ; 1.|]) (sqrt 2.) ( /. );;
let haar_filter2 = Vect.vect_of_array [|1. ; 1.|];;

let decompo_wt_matrix img_m it =
	fwt_or_2d 0 img_m it haar_filter;;

let decompo_wt img it =
	let img_matrix = Pixel_matrix.read_bmp img in
	let gray_matrix = Pixel_matrix.gray_levels img_matrix in
	let decompo = decompo_wt_matrix gray_matrix it in
	decompo;;

let decompo_wt_to_img img it gray_lev name_target =
	let img_matrix = Pixel_matrix.read_bmp img in
	
	if gray_lev then
		let gray_matrix = Pixel_matrix.gray_levels img_matrix in
		let decompo = decompo_wt_matrix gray_matrix it in
		let comp_matrix = Pixel_matrix.intmatrix_of_matrix decompo in

		Pixel_matrix.write_bmp name_target (Pixel_matrix.combine_filters comp_matrix comp_matrix comp_matrix)
	else
		let target_red = Pixel_matrix.red_filter img_matrix
		and target_blue = Pixel_matrix.blue_filter img_matrix
		and target_green = Pixel_matrix.green_filter img_matrix in

		let decompo_red = decompo_wt_matrix target_red it
		and decompo_blue = decompo_wt_matrix target_blue it
		and decompo_green = decompo_wt_matrix target_green it in
		
		let comp_matrix_red = Pixel_matrix.intmatrix_of_matrix decompo_red
		and comp_matrix_blue = Pixel_matrix.intmatrix_of_matrix decompo_blue
		and comp_matrix_green = Pixel_matrix.intmatrix_of_matrix decompo_green in

		Pixel_matrix.write_bmp name_target (Pixel_matrix.combine_filters comp_matrix_red comp_matrix_green comp_matrix_blue);;

#use "sys.ml";;

let rec calc_indice_div it (n, m) =
	if it = 0 then
		(n, m)
	else
		calc_indice_div (it - 1) (n / 2, m / 2)

let rec calc_indice_mult it (n, m) =
	if it = 0 then
		(n, m)
	else
		calc_indice_mult (it - 1) (n * 2, m * 2)

let txt_of_matrix mat it filename =
	let (n, m) = Matrix.dim mat in
	let (size_n, size_m) = calc_indice_div it (n, m) in
	let extr_mat = Matrix.extr_matrix mat (0, 0) (size_n, size_m) in
	let file = open_out filename in
	output_string file ((string_of_int it) ^ "\n");
	for i = 0 to n - 1 do
		for j = 0 to m - 1 do
			let mat_i_j = Matrix.value mat (i, j) in
			if (i >= size_n || j >= size_m) && mat_i_j <> 0 then
				let str = (string_of_int i) ^ " " ^ (string_of_int j) ^ " " ^ (string_of_int mat_i_j) ^ "\n" in
				output_string file str
		done
	done;
	close_out file;
	extr_mat

let decompo_line line =
	let tab = Array.make 3 0 in
	let n = String.length line in
	let str = ref "" in
	let j = ref 0 in
	for i = 0 to n - 1 do
		if line.[i] <> ' ' then
			(str := !str ^ (String.make 1 line.[i]);
			if i = n - 1 then
				tab.(!j) <- int_of_string !str)
		else
			(tab.(!j) <- int_of_string !str;
			str := "";
			incr j)
	done;
	(tab.(0), tab.(1), tab.(2)) 

let matrix_of_txt mat filename =
	let file = open_in filename in
	let mult = int_of_string (input_line file) in
	let (size_n, size_m) = Matrix.dim mat in
	let (n, m) = calc_indice_mult mult (size_n, size_m) in
	
	let complete_mat = Matrix.create 0. n m in
	Matrix.put_in complete_mat mat (0, 0); 
	
	let end_of_file = ref false in

	while (not !end_of_file) do
		try
			let line = input_line file in
			let (i, j, value) = decompo_line line in
			Matrix.affect complete_mat (float_of_int value) (i, j)
		with
			| End_of_file -> end_of_file := true
	done;
	close_in file;
	
	(complete_mat, mult);;

let compresser_img img it filename_target =
	let mat = decompo_wt img it in
	let intmat = Pixel_matrix.intmatrix_of_matrix mat in
	let extr_mat = txt_of_matrix intmat it filename_target in
	Pixel_matrix.write_bmp (filename_target ^ ".bmp") (Pixel_matrix.combine_filters extr_mat extr_mat extr_mat);;

let decompresser_img img file target =
	let mat = Pixel_matrix.read_bmp img in
	let gray_mat = Pixel_matrix.gray_levels mat in
	let (mat_recomp, it) = matrix_of_txt gray_mat file in
	let decomp = fwt_or_2d 1 mat_recomp it haar_filter2 in
	let intdecomp = Pixel_matrix.intmatrix_of_matrix decomp in
	let combine = Pixel_matrix.combine_filters intdecomp intdecomp intdecomp in
	Pixel_matrix.write_bmp target combine;;

let comp_decomp img it target =
	let mat = Pixel_matrix.read_bmp img in
	let gray_mat = Pixel_matrix.gray_levels mat in
	let comp = fwt_or_2d 0 gray_mat it haar_filter in
	
	let decomp = fwt_or_2d 1 comp it haar_filter2 in
	let intdecomp = Pixel_matrix.intmatrix_of_matrix decomp in
	let combine = Pixel_matrix.combine_filters intdecomp intdecomp intdecomp in
	Pixel_matrix.write_bmp target combine;
	let diff = Matrix.sum gray_mat decomp ( -. ) in
	Matrix.norm_sup diff abs

let error img it save_diff i =
	let mat = Pixel_matrix.read_bmp img in
	let original = Pixel_matrix.gray_levels mat in
	let comp = decompo_wt_matrix original it in

	let intcomp = Pixel_matrix.intmatrix_of_matrix comp in
	let extr = txt_of_matrix intcomp it ("text" ^ (string_of_int (i + 1))) in
	
	let pixextr = Pixel_matrix.combine_filters extr extr extr in
	let extrfloat = Pixel_matrix.gray_levels pixextr in

	let (recomp, _) = matrix_of_txt extrfloat ("text" ^ (string_of_int (i + 1))) in

	let decomp = fwt_or_2d 1 recomp it haar_filter2 in

	let denorm_decomp = denormalise decomp (Matrix.dim original) in

	let intoriginal = Pixel_matrix.intmatrix_of_matrix original
	and intdecomp = Pixel_matrix.intmatrix_of_matrix denorm_decomp in

	let mat_diff = Matrix.sum intoriginal intdecomp ( - ) in
	if save_diff then
		(let pix_mat_diff = Pixel_matrix.combine_filters mat_diff mat_diff mat_diff in
		let pix_mat_decomp = Pixel_matrix.combine_filters intdecomp intdecomp intdecomp in
		let filename = "ara.bmp" in
		Pixel_matrix.write_bmp ("error_" ^ filename) ((*Pixel_matrix.negative*) (pix_mat_diff));
		Pixel_matrix.write_bmp ("decomp_" ^ filename) pix_mat_decomp);
	Matrix.norm_eucli mat_diff ( + ) ( * ) 0

let error_tab tab_img it save_diff =
	let n = Array.length tab_img in
	let tab_error = Array.make n 0 in
	for i = 0 to n - 1 do
		tab_error.(i) <- error tab_img.(i) it save_diff i
	done;
	tab_error

let print_str str =
	for i = 0 to (String.length str - 1) do
		print_char str.[i]
	done

let print_str_new_line str = 
	print_str str;
	print_char '\n'

let print_str_tab str =
	print_char '\t';
	print_str str

let initialize_tab_img () =
	let t = Array.make 10 "" in
	for i = 0 to 9 do
		t.(i) <- "img/lena" ^ (string_of_int (i + 1)) ^ "00.bmp"
	done;
	t

let compresser_img_tab tab it =
	for i = 0 to (Array.length tab - 1) do
		let filename_target = "tgt/target_" ^ (string_of_int (i + 1)) ^ "00" in
		compresser_img tab.(i) it filename_target
	done

let gray_levels_of_colors tab =
	for i = 0 to (Array.length tab - 1) do
		let pixelmat = Pixel_matrix.read_bmp tab.(i) in
		let gray = Pixel_matrix.gray_levels pixelmat in
		let intgray = Pixel_matrix.intmatrix_of_matrix gray in
		let combine = Pixel_matrix.combine_filters intgray intgray intgray in
		Pixel_matrix.write_bmp ("lena" ^ (string_of_int (i + 1)) ^ "00.bmp") combine
	done

let calc_stats tab_img it threshold =
	print_str "Initialize calc_stats ...";
	let n = Array.length tab_img in
	let stats_read_bmp = Array.make n 0. 
	and stats_gray_levels = Array.make n 0.
	and size_matrix = Array.make n (0, 0)
	and stats_decompo = Array.make n 0.
	and stats_intmat_of_mat = Array.make n 0.
	and stats_combine_filters = Array.make n 0.
	and stats_write_bmp = Array.make n 0. in
	print_str_new_line "done";

	for i = 0 to n - 1 do
		let avancement = ("(" ^ (string_of_int (i + 1)) ^ "/" ^ (string_of_int n) ^ ")") in
		print_str_tab ("Read bmp " ^ avancement ^ " ...");
		let actual_time = ref (time ()) in

		let img_matrix = Pixel_matrix.read_bmp tab_img.(i) in
		stats_read_bmp.(i) <- time () -. !actual_time;
		print_str_new_line "done";
		
		print_str_tab ("Gray transform picture " ^ avancement ^ " ...");
		actual_time := time ();

		let gray_matrix = Pixel_matrix.gray_levels img_matrix in
		stats_gray_levels.(i) <- time () -. !actual_time;
		
		size_matrix.(i) <- Matrix.dim gray_matrix;
		print_str_new_line "done";

		print_str_tab ("Decompo picture " ^ avancement ^ " ...");

		actual_time := time ();

		let decompo = decompo_wt_matrix gray_matrix it in
		stats_decompo.(i) <- time () -. !actual_time;
	
		print_str_new_line "done";

		let extr = decompo in
		(*let (n, m) = Matrix.dim decompo in
		let extr = Matrix.extr_matrix decompo (0, 0) (n - n /2, m - m/2) in
		size_matrix.(i) <- Matrix.dim extr;*)

		print_str_tab ("Transform to int matrix " ^ avancement ^ " ...");
		actual_time := time ();

		let comp_matrix = Pixel_matrix.intmatrix_of_matrix extr in
		stats_intmat_of_mat.(i) <- time () -. !actual_time;

		print_str_new_line "done";

		print_str_tab ("Transform to pixel_matrix " ^ avancement ^ " ...");
		actual_time := time ();

		let combine = Pixel_matrix.combine_filters comp_matrix comp_matrix comp_matrix in
		stats_combine_filters.(i) <- time () -. !actual_time;
		
		print_str_new_line "done";

		print_str_tab ("Save wt picture " ^ avancement ^ " ...");
		actual_time := time ();
		
		let name_target = "tgt/target_" ^ (string_of_int (i + 1)) ^ "00.bmp" in
		Pixel_matrix.write_bmp name_target combine;
		
		print_str_new_line "done";
		print_char '\n';
		stats_write_bmp.(i) <- time () -. !actual_time
	done;

	(stats_read_bmp, stats_gray_levels, size_matrix, stats_decompo,
		stats_intmat_of_mat, stats_combine_filters, stats_write_bmp)
