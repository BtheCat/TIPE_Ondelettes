(*-------------------------------------------------------------------*)
(* Lecture/écriture d'images BMP 24 bits *)

(* Marc Lorenzi *)
(* 20/09/2008 *)
(*-------------------------------------------------------------------*) 

#load "graphics.cma";;
open Graphics;;

(*-------------------------------------------------------------------*)
(* word = 2 bytes, dword = 4 bytes *)
(* Aucune différence en Caml *)

type word  = int;;
type dword = int;;

(*-------------------------------------------------------------------*)
(* En-tête de fichier BMP : Bitmap File Header *)

type bitmapFileHeader = {
  bfType      : string;
  bfSize      : dword;
  bfReserved1 : word;
  bfReserved2 : word;
  bfOffBits   : dword;
};;

(*-------------------------------------------------------------------*)
(* En-tête de fichier BMP : Bitmap Info Header *)

type bitmapInfoHeader = {
  biSize          : dword;
  biWidth         : dword;
  biHeight        : dword;
  biPlanes        : word;
  biBitCount      : word;
  biCompression   : dword;
  biSizeImage     : dword;
  biXPelsPerMeter : dword;
  biYPelsPerMeter : dword;
  biClrUsed       : dword;
  biClrImportant  : dword;
};;

(*-------------------------------------------------------------------*)
(* Lecture/écriture des deux premiers octets du fichier (normalement "BM") *)

let read_type channel = 
  let s = "  " in
  s.[0] <- input_char channel;
  s.[1] <- input_char channel;
  s;;
 
let write_type channel = 
  output_char channel 'B';
  output_char channel 'M';;
 
(*-------------------------------------------------------------------*)
(* Lecture/Ecriture d'un Word et d'un DWord.*)
(* Dans un fichier BMP, les octets les moins significatifs dont stockés
en premier (little endian) *) 

let read_dword channel =
  let a = input_byte channel in
  let b = input_byte channel in
  let c = input_byte channel in
  let d = input_byte channel in
  (d lsl 24) lor (c lsl 16) lor (b lsl 8) lor a;;

let write_dword channel x =
  let a = x lsr 24
  and b = (x lsr 16) land 255
  and c = (x lsr 8) land 255
  and d = x land 255 in
  output_byte channel d;
  output_byte channel c;
  output_byte channel b;
  output_byte channel a;;
  
let read_word channel =
  let a = input_byte channel in
  let b = input_byte channel in
  (b lsl 8) lor a;;

let write_word channel x =
  let c = (x lsr 8) land 255
  and d = x land 255 in
  output_byte channel d;
  output_byte channel c;;  

(*-------------------------------------------------------------------*)
(* Lecture/Ecriture du BitmapFileHeader *)

let read_file_header channel =
  let t = read_type channel in
  let sz = read_dword channel in
  let r1 = read_word channel in
  let r2 = read_word channel in
  let off = read_dword channel in
  {
    bfType = t;
    bfSize = sz;
    bfReserved1 = r1;
    bfReserved2 = r2;
    bfOffBits = off;
  };;
  
let write_file_header channel fh =
  write_type channel;
  write_dword channel fh.bfSize;
  write_word channel fh.bfReserved1;
  write_word channel fh.bfReserved2;
  write_dword channel fh.bfOffBits;;


(*-------------------------------------------------------------------*)
(* Lecture/Ecriture du BitmapInfoHeader *)

let read_info_header channel =
  let sz = read_dword channel in
  let w = read_dword channel in
  let h = read_dword channel in
  let pl = read_word channel in
  let bc = read_word channel in
  let compr = read_dword channel in
  let szim = read_dword channel in
  let xpm = read_dword channel in
  let ypm = read_dword channel in
  let clru = read_dword channel in
  let clri = read_dword channel in
  {
    biSize = sz;
    biWidth = w;
    biHeight = h;
    biPlanes = pl;
    biBitCount = bc;
    biCompression = compr;
    biSizeImage = szim;
    biXPelsPerMeter= xpm;
    biYPelsPerMeter= ypm;
    biClrUsed = clru;
    biClrImportant = clri;
  };; 
  
let write_info_header channel ih =
  write_dword channel ih.biSize;
  write_dword channel ih.biWidth;
  write_dword channel ih.biHeight;
  write_word channel ih.biPlanes;
  write_word channel ih.biBitCount;
  write_dword channel ih.biCompression;
  write_dword channel ih.biSizeImage;
  write_dword channel ih.biXPelsPerMeter;
  write_dword channel ih.biYPelsPerMeter;
  write_dword channel ih.biClrUsed;
  write_dword channel ih.biClrImportant;;

(*-------------------------------------------------------------------*)
(* Lecture des pixels. Renvoie une matrice m de dimensions w x h où 
(w, h) est la taille de l'image. m.(i).(j) contient le triplet (r,g,b) 
des composantes couleurs du pixel situé àux coordonnées (j, i) de l'image.
La coordonnée 0,0 est en général le coin inférieur gauche, à moins que le
champ biHeight du bitmapInfoHeader soit une valeur négative. Ce cas n'est
pas traité ici.
Unique subtilité : le nombre d'octets dans une "ligne" du fichier image
doit être un multiple de 4. Si le nombre réel de pixels ne vérifie pas cette
condition, on complète par des zéros.*)

(* Multiple de 4 immédiatement supérieur ou égal à w *)  

let offset w =
  let r = (3 * w) mod 4 in
  if r = 0 then 0
  else 4 - r;;

(* val read_pixels : bitmapFileHeader -> bimapInfoHeader -> in_channel -> unit *)

type pixel = int * int * int;;
type pixel_matrix = pixel array array;;

let read_pixels fh ih channel =
  let w = ih.biWidth
  and h = ih.biHeight in
  let offs = offset w in
  let m = Array.make_matrix w h (0,0,0) in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      let b = input_byte channel in
      let g = input_byte channel in
      let r = input_byte channel in
      m.(i).(j) <- (r, g, b)
    done;
    for i = 1 to offs do
        let _ = input_byte channel in ()
    done
  done;
  (m:pixel_matrix);;

(* val write_pixels : out_channel -> (int*int*int) array array -> unit *)

let write_pixels channel (m:pixel_matrix) =
  let w = Array.length m
  and h = Array.length m.(0) in
  let offs = offset w in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      let r, g, b = m.(i).(j) in
      output_byte channel b;
      output_byte channel g;
      output_byte channel r;
    done;
    for i = 1 to offs do
        output_byte channel 0
    done
  done;;

(*-------------------------------------------------------------------*)
(* Lecture d'une image BMP *)

(* val read_bmp : string -> (int*int*int) array array *)
      
let read_bmp filename =
  let channel = open_in_bin filename in
  let fh = read_file_header channel in
  let ih = read_info_header channel in
  let m = read_pixels fh ih channel in
  close_in channel;
  m;;

(*-------------------------------------------------------------------*)
(* Affichage sur la sortie standard des en-têtes d'une image *)

let print_data s x =
  print_string s;
  print_int x;
  print_string "\n";;

let print_headers filename = 
  let channel = open_in_bin filename in
  let fh = read_file_header channel in
  let ih = read_info_header channel in
  close_in channel;
  print_string "BitmapFileHeader...\n";
  print_string("=> bfType          : " ^ fh.bfType ^ "\n");
  print_data "=> bfSize          : " fh.bfSize;
  print_data "=> bfReserved1     : " fh.bfReserved1;
  print_data "=> bfReserved2     : " fh.bfReserved2;
  print_data "=> bfOffBits       : " fh.bfOffBits;
  print_newline();
  print_string  "bitmapInfoHeader...\n";
  print_data "=> biSize          : " ih.biSize;
  print_data "=> biWidth         : " ih.biWidth;
  print_data "=> biHeight        : " ih.biHeight;
  print_data "=> biPlanes        : " ih.biPlanes;
  print_data "=> biBitCount      : " ih.biBitCount;
  print_data "=> biCompression   : " ih.biCompression;
  print_data "=> biSizeImage     : " ih.biSizeImage;
  print_data "=> biXPelsPerMeter : " ih.biXPelsPerMeter;
  print_data "=> biYPelsPerMeter : " ih.biYPelsPerMeter;
  print_data "=> biClrUsed       : " ih.biClrUsed;
  print_data "=> biClrImportant  : " ih.biClrImportant;
  print_newline();;
  
(* Création des en-têtes de fichier BMP pour l'écriture *)  

let make_file_header w h = 
  let off = offset w in
  {
    bfType = "BM";
    bfSize = (w + off) * h * 3 + 54;
    bfReserved1  = 0;
    bfReserved2 = 0;
    bfOffBits = 54;
  };;
  
let make_info_header w h = 
  let off = offset w in
  {
    biSize = 40;
    biWidth = w;
    biHeight  = h;
    biPlanes  = 1;
    biBitCount = 24;
    biCompression = 0;
    biSizeImage = (w + off) * h * 3;
    biXPelsPerMeter = 0;
    biYPelsPerMeter = 0;
    biClrUsed = 0;
    biClrImportant = 0;
  };;
  
(* val write_bmp : string -> (int*int*int) array array -> unit *)

let write_bmp filename m =
  let channel = open_out_bin filename in
  let w = Array.length m
  and h = Array.length m.(0) in
  let fh = make_file_header w h
  and ih = make_info_header w h in
  write_file_header channel fh;
  write_info_header channel ih;
  write_pixels channel m;
  close_out channel;;
  
(*-------------------------------------------------------------------*)
(* Afficher l'image dans une fenêtre graphique *)
(* Un appui sur une touche ferme la fenêtre *)

(* !! Sous Windows, le fait de cliquer sur la case de fermeture de
la fenêtre graphique N'ARRETE PAS LE PROGRAMME. Ennuis en perspective,
donc. Conclusion : appuyer sur une touche pour sortir proprement du
programme. *)

let show_me (m:pixel_matrix) =
  let w = Array.length m
  and h = Array.length m.(0) in
  let s = " " ^ (string_of_int (w + 20)) ^ "x" ^ 
          (string_of_int (h + 45)) ^ "+0+0" in
  
  open_graph s;
  let window_title = "Pixel Matrix Viewer (" ^
                      (string_of_int w) ^ "x" ^ 
                      (string_of_int h) ^ ")" in
  set_window_title (window_title);
  (*clear_graph();
  auto_synchronize false;*)
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      let (r, g, b) = m.(i).(j) in
        set_color (rgb r g b);
        plot (i + 5) (j + 5)
    done
  done;
  (*synchronize();*)
  let _ = read_key() in close_graph();;

(*-------------------------------------------------------------------*)  
(* Petits utilitaires *)

(* Dimensions d'une matrice de pixels *)

let dimensions (m:pixel_matrix) = 
  (Array.length m, Array.length m.(0));;

(* Créer une matrice de pixels de taille w x h *)

let create_pixel_matrix w h =
  let m = Array.make_matrix w h (0, 0, 0) in
  (m:pixel_matrix);;
  
(* Copier une matrice de pixels *)

let copy_pixel_matrix (m:pixel_matrix) =
  let (w, h) = dimensions m in
  let m1 = create_pixel_matrix w h in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      m1.(i).(j) <- m.(i).(j)
    done
  done;
  (m1:pixel_matrix);;

(* Négatif d'une matrice de pixels *)

let negative m =
  let (w, h) = dimensions m in
  let m1 = create_pixel_matrix w h in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let (r, g, b) = m.(i).(j) in
      m1.(i).(j) <- (255 - r, 255 - g, 255 - b)
    done
  done;
  m1;;

(* Conversion en niveaux de gris *)

let gray_levels m =  
  let (w, h) = dimensions m in
  let m1 = create_pixel_matrix w h in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let (r, g, b) = m.(i).(j) in
      let c = (r + g + b) / 3 in
      m1.(i).(j) <- (c, c, c)
    done
  done;
  m1;;
  
(* Symétrie gauche <-> droite *)

let mirror_left_right m =
  let (w, h) = dimensions m in
  let m1 = create_pixel_matrix w h in 
  for j = 0 to h - 1 do
    for i = 0 to (w - 1) / 2 do
      m1.(i).(j) <- m.(w - i - 1).(j);
      m1.(w - i - 1).(j) <- m.(i).(j)
    done
  done;
  m1;;

(* Symétrie haut <-> bas *)

let mirror_up_down m =
  let (w, h) = dimensions m in
  let m1 = create_pixel_matrix w h in 
  for i = 0 to w - 1 do
    for j = 0 to (h - 1) / 2 do
      m1.(i).(j) <- m.(i).(h - j - 1);
      m1.(i).(h - j - 1) <- m.(i).(j)
    done
  done;
  m1;;

(*-------------------------------------------------------------------*)
(* Tests *)

(*let run_tests() =
  let image = "ara.bmp" in
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

run_tests();;*)
