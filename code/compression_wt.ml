#use "array.ml";;
#use "fwt.ml";;

let haar_filter = Vect.vect_of_array [|1. /. 2. ; 1. /. 2.|];;
let haar_filter2 = Vect.vect_of_array [|1. ; 1.|];;

let decompo_wt_matrix img_m it =
  fwt 0 img_m it haar_filter;;

let decompo_wt img it =
  let img_matrix = Pixel_matrix.read_bmp img in
  let gray_matrix = Pixel_matrix.gray_levels img_matrix in
  let decompo = decompo_wt_matrix gray_matrix it in
  decompo;;
		
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
	let str = (string_of_int i) ^ " " ^ (string_of_int j) ^ 
		  " " ^ (string_of_int mat_i_j) ^ "\n" in
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
  Pixel_matrix.write_bmp (filename_target ^ ".bmp") 
      (Pixel_matrix.combine_filters extr_mat extr_mat extr_mat);;

let decompresser_img img file target =
  let mat = Pixel_matrix.read_bmp img in
  let gray_mat = Pixel_matrix.gray_levels mat in
  let (mat_recomp, it) = matrix_of_txt gray_mat file in
  let decomp = fwt 1 mat_recomp it haar_filter2 in
  let intdecomp = Pixel_matrix.intmatrix_of_matrix decomp in
  let combine = Pixel_matrix.combine_filters intdecomp intdecomp intdecomp in
  Pixel_matrix.write_bmp target combine;;

let error img it i =
  let mat = Pixel_matrix.read_bmp img in
  let original = Pixel_matrix.gray_levels mat in
  let comp = decompo_wt_matrix original it in

  let intcomp = Pixel_matrix.intmatrix_of_matrix comp in
  let extr = txt_of_matrix intcomp it ("text" ^ (string_of_int (i + 1))) in
  
  let pixextr = Pixel_matrix.combine_filters extr extr extr in
  let extrfloat = Pixel_matrix.gray_levels pixextr in

  let (recomp, _) = matrix_of_txt extrfloat ("text" ^ (string_of_int (i + 1))) in

  let decomp = fwt 1 recomp it haar_filter2 in

  let denorm_decomp = denormalise decomp (Matrix.dim original) in

  let intoriginal = Pixel_matrix.intmatrix_of_matrix original
  and intdecomp = Pixel_matrix.intmatrix_of_matrix denorm_decomp in

  let mat_diff = Matrix.sum intoriginal intdecomp ( - ) in
  Matrix.norm_eucli mat_diff ( + ) ( * ) 0
