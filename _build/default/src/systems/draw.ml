open Component_defs


class type drawable =
  object
  inherit box
  inherit position
  inherit color
end

type t = drawable

let init () = ()

let white = Gfx.color 40 40 40 255
let update _dt el =
  let win = Game_state.get_window () in
  let ctx = Gfx.get_context win in
  let win_surf = Gfx.get_surface win in
  let w, h = Gfx.get_context_logical_size ctx in
  let () = Gfx.set_color ctx white in
  let () = Gfx.fill_rect ctx win_surf 0 0 w h in
  let plypos = (Game_state.get_player())#position#get in

  Seq.iter (fun (e : t) ->
    let Vector.{ x; y } = e # position # get in
    let Rect.{width; height} = e # box # get in
    let x = x -. plypos.x +. 200.0 in
    let color = e # color # get in 
    Gfx.set_color ctx color;
    Gfx.fill_rect ctx win_surf (int_of_float x) (int_of_float y) width height     
    ) el;
    Gfx.commit ctx