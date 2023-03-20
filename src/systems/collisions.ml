open Component_defs

class type collidable =
  object
    inherit velocity
    inherit position
    inherit box
    inherit mass
    inherit on_jump
    inherit block_type
    inherit first_collide
  end

type t = collidable;;
type blockSide = Top | Bottom | Left | Right;;

let init () = ()

let get_side (ply: player) (solid: collidable) =
  let size = solid#box#get in
  let normVec1 = Vector.normalize (Vector.sub ply#position#get solid#position#get) in
  (* Haut à gauche*)
  let normVec2 = Vector.normalize (
    Vector.sub (
      Vector.add solid#position#get (Vector.{x = float_of_int (-size.width / 2); y = float_of_int (-size.height / 2)})) 
      solid#position#get
  ) in
    
  if (normVec2.y > normVec1.y) then
    Top
  else if (normVec1.y > (-. normVec2.y)) then
    Bottom
  else if (normVec1.x < normVec2.x) then
    Left
  else
    Right;;

let onCollision (b1: collidable) (b2: collidable) =
  let t1 = b1#block_type#get in
  let t2 = b2#block_type#get in

  if(t1 != Block_type.Player && t2 != Block_type.Player) then ();

  let ply = Game_state.get_player() in
  let solid = (if t1 != Block_type.Player then b1 else b2) in
  let fc = not solid#first_collide#get in

  begin
    match (solid#block_type#get) with
      | Block_type.Spikes -> ply#position#set (Vector.{x = 0.0; y = 200.0})
      (*
      | Block_type.ReverseGravity ->
        if fc then begin
          ply#inverted_gravity#set (not ply#inverted_gravity#get);
          ply#position#set Vector.{x=ply#position#get.x; y=ply#position#get.y +. 0.5}
        end
      | Block_type.DoubleJump -> if fc then ply#on_jump#set = 1*)

      | Block_type.ReverseGravity | Block_type.Solid ->  
        let side = get_side ply solid in
        let on_ground = (side == Top && not ply#inverted_gravity#get) || (side == Bottom && ply#inverted_gravity#get) in
        if on_ground then begin
          ply#on_jump#set 1;
          
          let ang = (int_of_float ply#rot#get) mod 90 in
          (*let rot = (if ang < 45 then Float.floor (ply#rot#get /. 90.0) else Float.ceil (ply#rot#get /. 90.0)) in

          ply#rot#set (rot *. 90.0);
          *)
          ply#rot#set 0.0;
        end
      | Block_type.DisableFlying -> if fc then ply#flying#set false
      | Block_type.EnableFlying -> if fc then ply#flying#set true
      | _ -> ()(*Level_load.set_level (Level_load.get_levelid() + 1)*);
  end;

  solid#first_collide#set true;;

let update _dt el =
  let plyPosX = (Game_state.get_player())#position#get in
  Seq.iteri
    (fun i (e1 : t) ->
      (* les composants du rectangle r1 *)
      let pos1 = e1#position#get in
      let box1 = e1#box#get in
      let v1 = e1#velocity#get in
      let m1 = e1#mass#get in

      if (pos1.x +. (float_of_int(box1.width)) < plyPosX.x) then
        ();

      Seq.iteri
        (fun j (e2 : t) ->
          let m2 = e2#mass#get in
          (* Une double boucle qui évite de comparer deux fois
             les objets : si on compare A et B, on ne compare pas B et A.
             Il faudra améliorer cela si on a beaucoup (> 30) objets simultanément.
          *)


          (*if j > i && (Float.is_finite m1 || Float.is_finite m2) then begin*)
          if (j > i) && (Float.is_finite m1 || Float.is_finite m2) then begin
            (* les composants du rectangle r2 *)
            let pos2 = e2#position#get in
            let box2 = e2#box#get in
            (* les vitesses *)
            let v2 = e2#velocity#get in
            (* [1] la soustraction de Minkowski *)
            let s_pos, s_rect = Rect.mdiff pos2 box2 pos1 box1 in
            (* [2] si intersection et un des objets et mobile, les objets rebondissent *)
            if
              Rect.has_origin s_pos s_rect
              && not (Vector.is_zero v1 && Vector.is_zero v2)
            then begin
              (* [3] le plus petit des vecteurs a b c d *)
              let a = Vector.{ x = s_pos.x; y = 0.0 } in
              let b = Vector.{ x = float s_rect.width +. s_pos.x; y = 0.0 } in
              let c = Vector.{ x = 0.0; y = s_pos.y } in
              let d = Vector.{ x = 0.0; y = float s_rect.height +. s_pos.y } in
              let n =
                List.fold_left
                  (fun min_v v ->
                    if Vector.norm v <= Vector.norm min_v then v else min_v)
                  d [ a; b; c ]
              in
              (*  [4] rapport des vitesses et déplacement des objets *)
              let n_v1 = Vector.norm v1 in
              let n_v2 = Vector.norm v2 in
              let s = 1.01 /. (n_v1 +. n_v2) in
              let n1 = n_v1 *. s in
              let n2 = n_v2 *. s in
              let delta_pos1 = Vector.mult n1 n in
              let delta_pos2 = Vector.mult (-.n2) n in
              let pos1 = Vector.add pos1 delta_pos1 in
              let pos2 = Vector.add pos2 delta_pos2 in
              let s_pos, s_rect = Rect.mdiff pos2 box2 pos1 box1 in
              if Rect.has_origin s_pos s_rect then begin
                Gfx.debug "%f, %f, %d x %d\n" s_pos.Vector.x s_pos.Vector.y
                  s_rect.Rect.width s_rect.Rect.height;
                (*assert false*)
              end;
              e1#position#set pos1;
              e2#position#set pos2;

              (* [5] On normalise n (on calcule un vecteur de même direction mais de norme 1) *)
              let n = Vector.normalize n in
              (* [6] Vitesse relative entre v2 et v1 *)
              let v = Vector.sub v1 v2 in

              (* Préparation au calcul de l'impulsion *)
              (* Elasticité fixe. En pratique, l'elasticité peut être stockée dans
                 les objets comme un composant : 1 pour la balle et les murs, 0.5 pour
                 des obstacles absorbants, 1.2 pour des obstacles rebondissant, … *)
              let e = 0.01 in
              (* normalisation des masses *)
              let m1, m2 =
                if Float.is_infinite m1 && Float.is_infinite m2 then
                  if n_v1 = 0.0 then (m1, 1.0)
                  else if n_v2 = 0.0 then (1.0, m2)
                  else (0.0, 0.0)
                else (m1, m2)
              in
              (* [7] calcul de l'impulsion
              let jbase = -.(1.0 +. e) *. Vector.dot v n in
              let m1divm2 = m1 /. m2 in
              let m2divm1 = m2 /. m1 in
              let j1 = jbase /. (1.0 +. m1divm2) in
              let j2 = jbase /. (1.0 +. m2divm1) in
              *)

              let j =
                -.(1.0 +. e) *. Vector.dot v n /. ((1. /. m1) +. (1. /. m2))
              in
              (* [8] calcul des nouvelles vitesses *)
              let new_v1 = Vector.add v1 (Vector.mult (j/. m1) n) in
              let new_v2 = Vector.sub v2 (Vector.mult (j/. m2) n) in
              (* [9] mise à jour des vitesses *)
              e1#velocity#set new_v1;
              e2#velocity#set new_v2;

              (* Une collision avec le joueur à eu lieu ?*)
              onCollision e1 e2;
            end;
          end)
        el)
    el