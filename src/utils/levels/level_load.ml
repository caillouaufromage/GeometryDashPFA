open Component_defs
open System_defs

class type b =
  object
    inherit position
    inherit box
    inherit color
    (* Question 2 *)

    inherit mass
    inherit velocity
    inherit sum_forces
    inherit on_jump
    inherit block_type
  end;;

  (*
type levelEntity = {
  position: Vector;
  size: (int * int);
};;

(* Level 1*)
type Level = {
  entities: b list;
};;

let level1 = 
let entities: b array = [||];;
*)

  