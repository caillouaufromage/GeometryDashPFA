type block_type = 
  Level_Solid | (* Fait rien, murs de la partie *)
  Solid | (* Solide, collision qui tue seulement si il touche la gauche et droite *)
  Player | (* Permet de vérifier rapidement si les 2 collisions concernent un joueur *)
  Spikes | (* Tue à chaque fois que le joueur le touche*)
  DoubleJump |
  ReverseGravity |
  Level_End (* Trigger de fin de niveau *)

  (*
let textures = [

]
let enum_to_int t =
  match t with
    | Player -> 1
    | Solid -> 2
    | Spikes -> 3
    | Level_End -> 4
    | Level_Solid -> 5

let get_texture ctx t =
  match t with
    | Player -> Gfx.load_image ctx "player.png"
    | Solid -> Gfx.load_image ctx "wall.png"
    | Spikes -> Gfx.load_image ctx "wall.png"
    | Level_End -> Gfx.load_image ctx "wall.png"
    | Level_Solid -> Gfx.load_image ctx "wall.png"
    *)