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
