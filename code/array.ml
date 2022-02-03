let range n =
  let t = Array.make n 0 in
  for i = 1 to n - 1 do
    t.(i) <- i
  done;
  t

let ceil i d =
  if i mod d = 0 then i / d
  else i / d + 1

module type VECT =
  sig
    type 'a vect

    val vect_of_array : 'a array -> 'a vect
    val array_of_vect : 'a vect -> 'a array
    val length : 'a vect -> int
    val create : 'a -> int -> 'a vect
    val create_empty : 'a -> 'a vect
    val make_matrix : 'a -> int -> int -> 'a vect vect
    val alternate_id : int -> 'a -> ('a -> 'a) -> 'a vect
	val extend : 'a vect -> int -> 'a -> 'a vect

    val affect : 'a vect -> 'a -> int -> unit
    val value : 'a vect -> int -> 'a
    val reverse : 'a vect -> 'a vect
    val concat : 'a vect -> 'a vect -> 'a vect
    val sum : 'a vect -> 'a vect -> ('a -> 'a -> 'a) -> 'a vect
    val prod : 'a vect -> 'a vect -> ('a -> 'a -> 'a) -> 'a vect

    val sub_vect : 'a vect -> int -> int -> 'a vect
    val put_in : 'a vect -> 'a vect -> int -> unit
    val dilate : 'a vect -> int -> 'a -> 'a vect
    val extr_vect : 'a vect -> int -> int -> int -> 'a vect

    val filter : 'a vect -> 'a vect -> 'a vect -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a) -> 'a -> 'a vect
  end;;

module Vect : VECT =
  struct
    type 'a vect = 'a array

    let vect_of_array t = t
    let array_of_vect v = v

    let length v = Array.length v
		
    let create x n = Array.make n x

    let create_empty x = Array.make 0 x

    let value v ind = v.(ind)

    let affect v x ind = v.(ind) <- x

    let make_matrix x n m = Array.make_matrix n m x

    let alternate_id n one opp =
      let v = create one n in
      for i = 0 to n - 1 do
	if i mod 2 = 1 then
	  affect v (opp one) i
      done; v

    let reverse v = 
      let n = length v in
      let v_rev = create (value v 0) n in
      for i = 0 to n - 1 do
	affect v_rev (value v (n - 1 - i)) i
      done; v_rev

    let concat v1 v2 = 
      let n = length v1 and p = length v2 in
      if n = 0 then v2
      else if p = 0 then v1
      else
	let v = create (value v1 0) (n + p) in
	for i = 1 to (n + p - 1) do
	  if i < n then 
	    affect v (value v1 i) i
	  else 
	    affect v (value v2 (i - n)) i
	done; v

    let sum v1 v2 sum_elem = 
      let n = length v1 in
      if not (n = length v2) then 
	failwith "Vect.sum"
      else
	let v = create (value v1 0) n in
	for i = 0 to n - 1 do
	  affect v (sum_elem (value v1 i) (value v2 i)) i
	done; v

    let prod v1 v2 prod_elem = 
      let n = length v1 in
      if not (n = length v2) then
	failwith "Vect.prod"
      else
	let v = create (value v1 0) n in
	for i = 0 to n - 1 do
	  affect v (prod_elem (value v1 i) (value v2 i)) i
	done; v

    let sub_vect v i j = 
      let v_sub = create (value v i) (j - i) in
      for k = 1 to j - i - 1 do
	affect v_sub (value v (k + i)) k
      done; v_sub

    let put_in v1 v2 i =
      let n = length v2 in
      if (i + n) > (length v1) then
	failwith "Vect.put_in"
      else
	for j = 0 to n - 1 do
	  affect v1 (value v2 j) (j + i)
	done

    let extend x m zero =
      let n = length x in
	  if m = 0 then x
	  else
		let y = create zero (n + m) in
		put_in y x 0;
		y

    let dilate v s zero = 
      let n = length v in
      let v_dil = create zero (n*s) in
      for i = 0 to n - 1 do
	affect v_dil (value v i) (s*i)
      done;
      v_dil
		
    let extr_vect v ind_beg diff_ind ind_end = 
      let n = ceil (ind_end - ind_beg) diff_ind in
      let v_ex = create (value v 0) n in
      let p = ref ind_beg and q = ref 0 in
      while !p < ind_end do
	affect v_ex (value v !p) !q;
	incr q; p := !p + diff_ind
      done; v_ex

    let filter b a x sum prod inv zero =
      let n = length x in
      let y = create (value x 0) n in
      let nb = length b in
			
      let b = (if n > nb then extend b (n - nb) zero else b) in	
			
      for i = 0 to n - 1 do
	let s = ref zero in
	for j = 0 to i do
	  s := sum !s (prod (value b j) (value x (i - j)))
	done;
	s := prod !s (inv (value a 0));
	affect y !s i
      done;
      y

  end

module type MATRIX =
  sig
    type 'a matrix

    val dim : 'a matrix -> (int * int)
    val create : 'a -> int -> int -> 'a matrix
    val matrix_of_vect : 'a Vect.vect -> 'a matrix

    val value : 'a matrix -> (int * int) -> 'a
    val affect : 'a matrix -> 'a -> (int * int) -> unit
    val vect : 'a matrix -> int -> 'a Vect.vect
    val affect_vect : 'a matrix -> 'a Vect.vect -> (int * int) -> unit
    val put_in : 'a matrix -> 'a matrix -> (int * int) -> unit
    val line : 'a matrix -> int -> 'a Vect.vect
    val affect_line : 'a matrix -> 'a Vect.vect -> (int * int) -> unit

    val sum : 'a matrix -> 'a matrix -> ('a -> 'a -> 'a) -> 'a matrix
    val prod_scal_cano : 'a matrix -> 'a matrix -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a
    val norm_eucli : 'a matrix -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a

    val extr_matrix : 'a matrix -> (int * int) -> (int * int) -> 'a matrix
  end;;

module Matrix : MATRIX =
  struct
    type 'a matrix = ('a Vect.vect) Vect.vect

    let value mat (i, j) = Vect.value (Vect.value mat i) j

    let affect mat x (i, j) = 
      let vect = Vect.value mat i in
      Vect.affect vect x j; Vect.affect mat vect i
		
    let dim mat = (Vect.length mat, Vect.length (Vect.value mat 0))
		
    let create x n m = Vect.make_matrix x n m 

    let matrix_of_vect v = Vect.create v 1

    let vect mat j = 
      let (n, m) = dim mat in
      let vect_mat = Vect.create (value mat (0, j)) n in
      for i = 1 to n - 1 do
	Vect.affect vect_mat (value mat (i, j)) i
      done; vect_mat

    let affect_vect mat vect (i, j) =
      for k = 0 to (Vect.length vect - 1) do
	affect mat (Vect.value vect k) (i + k, j)
      done

    let line mat i = Vect.value mat i

    let affect_line mat vect (i, ind_beg) =
      let line_mat = line mat i in
      Vect.put_in line_mat vect ind_beg;
      Vect.affect mat line_mat i

    let put_in mat1 mat2 (i, j) =
      for k = 0 to fst (dim mat2) - 1 do
	affect_line mat1 (line mat2 k) (i + k, j)
      done
		
    let sum mat1 mat2 sum_elem =
      let size = dim mat1 in
      if not (size = dim mat2) then
	failwith "Matrix.sum"
      else
	let mat = create (value mat1 (0, 0)) (fst size) (snd size) in
	for i = 0 to fst size - 1 do
	  affect_line mat (Vect.sum (line mat1 i) (line mat2 i) sum_elem) (i, 0)
	done; 
	mat

    let prod_scal_cano mat1 mat2 sum_elem prod_elem zero =
      let (n, m) = dim mat1 in
      if (n, m) <> dim mat2 then
	failwith "Matrix.prod_scal_cano"
      else
	let prod_scal = ref zero in
	for i = 0 to n - 1 do
	  for j = 0 to m - 1 do
	    prod_scal := sum_elem !prod_scal (prod_elem (value mat1 (i, j)) (value mat2 (i, j)))
	  done
	done;
	!prod_scal				

    let norm_eucli mat sum_elem prod_elem zero =
      prod_scal_cano mat mat sum_elem prod_elem zero

    let extr_matrix mat (i, j) (n2, m2) =
      let (n, m) = dim mat in
      if (i + n2 > n || j + m2 > m) then
	failwith "Matrix.extr_matrix";
      let mat_extr = create (value mat (i, j)) n2 m2 in
      for k = 0 to n2 - 1 do
	for l = 0 to m2 - 1 do
	  affect mat_extr (value mat (i + k, j + l)) (k, l)
	done
      done;
      mat_extr

  end;;

#use "bmp.ml"

module type PIXEL_MATRIX =
  sig
    type pixel
    type pixel_matrix
	
    val barycenter : pixel -> float

    val read_pixels : bitmapFileHeader -> bitmapInfoHeader -> in_channel -> pixel_matrix
    val write_pixels : out_channel -> pixel_matrix -> unit
    val read_bmp : string -> pixel_matrix
    val write_bmp : string -> pixel_matrix -> unit

    val intmatrix_of_matrix : float Matrix.matrix -> int Matrix.matrix

    val gray_levels : pixel_matrix -> float Matrix.matrix
    val pick_color : pixel -> int -> int
    val extr_uplet : pixel_matrix -> int -> float Matrix.matrix
    val red_filter : pixel_matrix -> float Matrix.matrix
    val blue_filter : pixel_matrix -> float Matrix.matrix
    val green_filter : pixel_matrix -> float Matrix.matrix
    val combine_filters : int Matrix.matrix -> int Matrix.matrix -> int Matrix.matrix -> pixel_matrix
  end;;

module Pixel_matrix : PIXEL_MATRIX =
  struct
    type pixel = (int * int * int)
    type pixel_matrix = pixel Matrix.matrix

    let barycenter (r, g, b) =
      ( (float_of_int r) +. (float_of_int g) +. (float_of_int b)) /. 3.
	    
    let read_pixels fh ih channel =
      let w = ih.biWidth
      and h = ih.biHeight in
      let offs = offset w in
      let m = Matrix.create (0, 0, 0) w h in
      for j = 0 to h - 1 do
	for i = 0 to w - 1 do
	  let b = input_byte channel in
	  let g = input_byte channel in
	  let r = input_byte channel in
	  Matrix.affect m (r, g, b) (i, j)
	done;
	for i = 1 to offs do
	  let _ = input_byte channel in ()
	done
      done;
      m

    let write_pixels channel m =
      let (w, h) = Matrix.dim m in
      let offs = offset w in
      for j = 0 to h - 1 do
	for i = 0 to w - 1 do
	  let r, g, b = Matrix.value m (i, j) in
	  output_byte channel b;
	  output_byte channel g;
	  output_byte channel r;
	done;
	for i = 1 to offs do
	  output_byte channel 0
	done
      done

    let read_bmp filename =
      let channel = open_in_bin filename in
      let fh = read_file_header channel in
      let ih = read_info_header channel in
      let m = read_pixels fh ih channel in
      close_in channel;
      m

    let write_bmp filename m =
      let channel = open_out_bin filename in
      let (w, h) = Matrix.dim m in
      let fh = make_file_header w h
      and ih = make_info_header w h in
      write_file_header channel fh;
      write_info_header channel ih;
      write_pixels channel m;
      close_out channel

    let intmatrix_of_matrix matrix =
      let (w, h) = Matrix.dim matrix in
      let intmatrix = Matrix.create 0 w h in
      for i = 0 to w - 1 do
	for j = 0 to h - 1 do
	  let matrix_i_j = Matrix.value matrix (i, j) in
	  if matrix_i_j > 255. then
	    Matrix.affect intmatrix 255 (i, j)
	  else if matrix_i_j < 0. then
	    Matrix.affect intmatrix 0 (i, j)
	  else
	    Matrix.affect intmatrix (int_of_float matrix_i_j) (i, j)
	done
      done;
      intmatrix

    let gray_levels m =
      let (w, h) = Matrix.dim m in
      let m1 = Matrix.create (0.) w h in
      for i = 0 to w - 1 do
	for j = 0 to h - 1 do
	  let p = Matrix.value m (i, j) in
	  let c = barycenter p in
	  Matrix.affect m1 c (i, j)
	done
      done;
      m1

    let pick_color (r, g, b) i =
      match i with
	| 1 -> r
	| 2 -> g
	| 3 -> b
	| _ -> failwith "Pixel_matrix.pick_uplet"

    let extr_uplet mat c = 
      let (w, h) = Matrix.dim mat in
      let m1 = Matrix.create (0.) w h in
      for i = 0 to w - 1 do
	for j = 0 to h - 1 do
	  let p = Matrix.value mat (i, j) in
	  let u = float_of_int (pick_color p c) in
	  Matrix.affect m1 u (i, j)
	done
      done;
      m1
	    
    let red_filter mat = extr_uplet mat 1
    let green_filter mat = extr_uplet mat 2
    let blue_filter mat = extr_uplet mat 3

    let combine_filters r_f g_f b_f =
      let (w, h) = Matrix.dim r_f in
      if (not ((w,h) = Matrix.dim g_f)) || (not ((w,h) = Matrix.dim b_f)) then 
	failwith "Pixel_matrix.combine_filters"
      else
	let m = Matrix.create (0, 0, 0) w h in
	for i = 0 to w - 1 do
	  for j = 0 to h - 1 do
	    let r = Matrix.value r_f (i, j)
	    and g = Matrix.value g_f (i, j)
	    and b = Matrix.value b_f (i, j)
	    in
	    Matrix.affect m (r, g, b) (i, j)
	  done
	done;
	m

  end
