open Component_defs

class type velPlayer =
  object
    inherit velocity
    inherit on_jump
  end

type t = velPlayer

let init () = ()

let keys = Hashtbl.create 16

let update _dt el =
  let ply = Game_state.get_player() in
  let () =
    match Gfx.poll_event () with
    | Gfx.NoEvent -> ()
    | Gfx.KeyDown s ->
      Hashtbl.replace keys s ()
    | Gfx.KeyUp s -> Hashtbl.remove keys s
  in
  
  (*ply#on_ground#set true;*)
  if Hashtbl.mem keys "c" && (ply#on_jump#get > 0 || ply#flying#get) then begin
      (*let f = Vector.add (Vec) {x=0.0; y=1.9475} in*)
      let f = ply#sum_forces#get in 
      let jumpValue = if ply#flying#get then 0.02 else 0.5 in
      let f = Vector.add f {x = 0.0; y = (if ply#inverted_gravity#get then jumpValue else -.jumpValue)} in
      ply#sum_forces#set f;
      ply#on_jump#set (ply#on_jump#get - 1)
  end;
  ();;