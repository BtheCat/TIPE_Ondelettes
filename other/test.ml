(*#load "graphics.cma";;
#use "compression_wt.ml";;

let run_tests image =
  print_headers image;

  let m = read_bmp image in
  show_me m;

  let m1 = negative m in
  write_bmp "__img1__.bmp" m1;
  show_me m1;

  let m1 = mirror_left_right m in
  write_bmp "__img2__.bmp" m1;
  show_me m1;
  
  let m1 = gray_levels m in
  write_bmp "__img4__.bmp" m1;
  show_me m1;
  
  let m1 = negative (gray_levels m) in
  write_bmp "__img5__.bmp" m1;
  show_me m1;
  
  show_me m;;

run_tests "ara.bmp";;*)

(*#use "array.ml";;
#use "fwt_or_bi.ml";;*)
#use "compression_wt.ml";;

let rec len_int n =
	if n < 0 then
		1 + len_int (- n)
	else if n < 10 then 1
	else 
		1 + len_int (n / 10);;

let rec len_float x =
	if x < 0. then
		1 + len_float (-. x)
	else if x < 10. then 1
	else
		1 + len_float (x /. 10.);;

let vect = Vect.vect_of_array [|0.;1.;2.;3.;4.;5.;6.;7.;8.;9.|];;
(*Vect.max_length_coef vect len_float;;
Vect.show vect print_int len_float 0;;
let vect2 = Vect.create 0 100;;
Vect.show vect2 print_int len_int 0;;
Vect.length vect2;;
Vect.array_of_vect vect2;;
Vect.array_of_vect (Vect.create_empty 0);;
let array_vect = Vect.duplicate vect 5;;
Vect.affect (array_vect.(0)) 4 0;;
Vect.array_of_vect (array_vect.(0));;
Vect.array_of_vect (array_vect.(1));;

let matrix = Vect.make_matrix 0 3 3;;
Vect.show (Vect.value matrix 1) print_int len_int 0;;

Vect.show (Vect.create_tab_ind 3 8) print_int len_int 0;;
let opp x = - x;;
Vect.show (Vect.alternate_id 5 1 opp) print_int len_int 0;;
Vect.value vect 3;;
Vect.array_of_vect (Vect.reverse vect);;
Vect.array_of_vect (Vect.concat vect vect);;
Vect.array_of_vect (Vect.sum vect vect ( + ));;
let vect3 = (Vect.prod vect vect ( * ));;
Vect.array_of_vect (Vect.sum_scal vect 1 ( + ));;
Vect.array_of_vect (Vect.prod_scal vect 2 ( * ));;
Vect.array_of_vect (Vect.sub_vect vect 0 5);;
Vect.put_in vect2 vect 90;;
Vect.put_in vect2 (Vect.dilate vect 10) 0;;
Vect.array_of_vect vect2;;
Vect.array_of_vect (Vect.extr_vect vect2 10 11 20);;
let vect_copy = Vect.copy vect [|0;1;2;3;4;5;6;7;8;9|];;
Vect.affect vect_copy 3 0;;
Vect.array_of_vect vect;;
Vect.array_of_vect vect_copy;;

Matrix.show (Matrix.join [|vect;vect;vect;vect3|]) print_int len_int;;
Matrix.show (Matrix.create 0 5 5) print_int len_int;;*)
let matrix = Matrix.duplicate vect (Vect.length vect);;
Matrix.show matrix print_float len_float;;
(*Matrix.affect matrix 4. (1, 0);;
Matrix.arrays_of_matrix (Matrix.matrix_of_vect vect);;
Matrix.show (Matrix.identity 5 1. 0.) print_float len_float;;
let id = Matrix.identity 5 1 0;;*)
(*let matrix2 = Matrix.duplicate vect (Vect.length vect);;
Matrix.put_in matrix2 id (2,5);;
Vect.array_of_vect (Matrix.line matrix2 5);;
Matrix.affect_line matrix2 vect (5, 0);;
Matrix.show matrix2 print_int len_int;;
Matrix.show (Matrix.transpo matrix2) print_int len_int;;
Matrix.trace matrix2 ( + ) 0;;
Matrix.show (Matrix.sum matrix2 matrix2 ( + )) print_int len_int;;
Matrix.value matrix2 (9, 8);;
Matrix.value matrix2 (8, 9);;
Matrix.affect matrix2 7 (9, 8);;
Matrix.value matrix2 (9, 8);;*)

(*let matrix_test = Pixel_matrix.gray_levels (Pixel_matrix.read_bmp "ara.bmp" );;*)

let b = Vect.vect_of_array [|1.;2.;3.;4.;5.|];;
let a = Vect.vect_of_array [|1.|];;
let x = Vect.vect_of_array [|6.;5.;4.;3.;2.;1.|];;
let inv x = 1. /. x;;

let vect_filter = Vect.filter b a x ( +. ) ( *. ) inv 0.;;
let array_filter = Vect.array_of_vect vect_filter;;

let h0 = Vect.create 1. 10;;
let g0 = Vect.prod (Vect.alternate_id 10 one op) h0 prod;;
