#use "array.ml"

let op x = -. x
let inv x = 1. /. x
let vect_1 = Vect.vect_of_array [|1.|]

let round_2 n =
  if n mod 2 = 0 then n/2
  else n/2 + 1

let rec pow x n = 
  if n = 0 then 1
  else 
    let y = pow x (n / 2) in
    if n mod 2 = 0 then
      y * y
    else 
      x * y * y

let normalise x (n, m) pow_2 =
  let offs_n = (if (n mod pow_2 = 0) then 0 else pow_2 - (n mod pow_2))
  and offs_m = (if (m mod pow_2 = 0) then 0 else pow_2 - (m mod pow_2)) in
  let y = Matrix.create 0. (n + offs_n) (m + offs_m) in
  Matrix.put_in y x (0, 0);
  y, (n + offs_n, m + offs_m)

let denormalise x (n, m) =
	Matrix.extr_matrix x (0, 0) (n, m)

let aco f x =
  let n = Vect.length x
  and p = Vect.length f in
  let xpadded = ref (Vect.create_empty (Vect.value x 0)) in
  (if p < n then
    xpadded := (Vect.concat x (Vect.sub_vect x 0 p))
  else
    let z = Vect.create 0. p in
    for i = 0 to p - 1 do
      let imod = (i mod n) in
      Vect.affect z (Vect.value x imod) i
    done;
    xpadded := (Vect.concat x z));
  let fflip = Vect.reverse f in
  let ypadded = Vect.filter fflip vect_1 !xpadded ( +. ) ( *. ) inv 0. in
  Vect.sub_vect ypadded (p - 1) (n + p - 1)

let hi_up x g0 = 
  let tmp = ref (Vect.dilate x 2 0.) in
  let len_tmp = Vect.length !tmp - 1 in
  tmp := Vect.concat (Vect.create (Vect.value !tmp len_tmp) 1) 
	(Vect.sub_vect !tmp 0 len_tmp);
  aco g0 !tmp

let lo_conv x qmf =
  let d = aco qmf x in
  Vect.extr_vect d 0 2 (Vect.length d - 1)

let ico f x =
  let n = Vect.length x 
  and p = Vect.length f in
  let xpadded = ref (Vect.create (Vect.value x 0) 1) in
  (if p <= n then
    xpadded := (Vect.concat (Vect.sub_vect x (n - p) n) x)
  else
    let z = Vect.create 0. p in
    for i = 0 to p - 1 do
      let imod = ( (p * n - p + i) mod n ) in
      Vect.affect z (Vect.value x imod) i
    done;
    xpadded := (Vect.concat z x));
  let ypadded = Vect.filter f vect_1 !xpadded ( +. ) ( *. ) inv 0. in
  Vect.sub_vect ypadded p (n + p)

let hi_conv s qmf = 
  let s2 = Vect.concat (Vect.sub_vect s 1 (Vect.length s)) 
	  (Vect.create (Vect.value s 0) 1) in
  let d = ico qmf s2 in
  Vect.extr_vect d 0 2 (Vect.length d - 1)

let fwt sens x l h0 =
  let len_h0 = Vect.length h0 in 
  let g0 = Vect.prod (Vect.alternate_id len_h0 1. op) h0 ( *. ) in

  if sens = 0 then
  (let wc, (n, m) = normalise x (Matrix.dim x) (pow 2 l) in
  let nc = ref n
  and mc = ref m in

  for jsqual = 1 to l do
    for ix = 0 to !nc - 1 do
      let row = Vect.sub_vect (Matrix.line wc ix) 0 !mc in
      Matrix.affect_line wc (lo_conv row h0) (ix, 0);
      Matrix.affect_line wc (hi_conv row g0) (ix, (!mc / 2));
    done;

    for iy = 0 to !mc - 1 do
      let row = Vect.sub_vect (Matrix.vect wc iy) 0 !nc in
      Matrix.affect_vect wc (hi_conv row g0) (!nc / 2, iy);
      Matrix.affect_vect wc (lo_conv row h0) (0, iy);
    done;

    nc := !nc / 2; mc := !mc / 2
  done; wc)

  else
    (let (n, m) = Matrix.dim x in
    let pow_2 = pow 2 (l - 1) in
    let nc = ref (n / pow_2)
    and mc = ref (m / pow_2) in

    for jsqual = 1 to l do
      for iy = 0 to !mc - 1 do
	Matrix.affect_vect x (Vect.sum 
		(ico h0 (Vect.dilate 
			    (Vect.sub_vect (Matrix.vect x iy) 0 (!nc / 2)) 
			    2 0.)) 
		(hi_up (Vect.sub_vect (Matrix.vect x iy) (!nc / 2) !nc) g0) 
		( +. )) (0, iy)
      done;

      for ix = 0 to !nc - 1 do
	Matrix.affect_line x (Vect.sum 
		(ico h0 (Vect.dilate 
			    (Vect.sub_vect (Matrix.line x ix) 0 (!mc / 2)) 
			    2 0.)) 
		(hi_up (Vect.sub_vect (Matrix.line x ix) (!mc / 2) !mc) g0) 
		( +. )) (ix, 0)
      done;

      nc := !nc * 2; mc := !mc * 2;
    done; x)
