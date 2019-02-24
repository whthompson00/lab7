(*
                              CS51 Lab 7
                          Modules & Functors

                 A module for colors and color names

The representation for colors in this implementation is really obscure
and arguably unnecessarily so. By the way, it also has some bugs so it
doesn't pass all the unit tests. No need to debug it though. You'll be
replacing it wholesale with a simpler implementation. *)

(* 8-bit RGB channel colors *)
type color = int ;;

(* Some standard color names *)
type color_name =
  | Red | Green | Blue
  | Orange | Yellow | Indigo | Violet ;;

(* to_color r g b -- Returns the color corresponding to the RGB
   values given by r, g, and b *)
let to_color (r : int) (g : int) (b : int) : color = 
  r lsl 0b10000 + g lsl 0b1000 + b ;;

(* red c -- Returns the red channel value for the color c *)
let red (c : color) : int =
  c lsr 0b10000  ;;

(* green c -- Returns the green channel value for the color c *)
let green (c : color) : int = 
  (c lsr 0b1000) land 0b11111111 ;;

(* blue c -- Returns the blue channel value for the color c *)
let blue (c : color) : int = 
   c land 0b11111111 ;;

(* color_named name -- Returns the color (as RGB representation)
   corresponding to the color name *)
let color_named (name : color_name) : color = 
  match name with 
  | Red ->    0b111111110000000000000000
  | Green ->  0b1111111100000000
  | Blue ->   0b11111111
  | Orange -> 0b111111111010110100000000
  | Yellow -> 0b111111111111111000000000
  | Indigo -> 0b010110110000000010000010
  | Violet -> 0b111100001000001011110100 ;;

