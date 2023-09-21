open Component_defs


class type drawable =
  object
    inherit box
    inherit position
    inherit color
    inherit block_type
    inherit rot
  end

type t = drawable
let textures: (int, Gfx.surface Gfx.resource) Hashtbl.t = Hashtbl.create 3;;
let bgTextures: (int, (Gfx.surface Gfx.resource * int)) Hashtbl.t = Hashtbl.create 3;;
let white = (Gfx.color 255 255 255 255);;
let black = (Gfx.color 40 40 0 255);;

let init () =
  let ctx = Gfx.get_context (Game_state.get_window()) in

  Hashtbl.add textures 1 (Gfx.load_image ctx "resources/player/run.png");
  Hashtbl.add textures 2 (Gfx.load_image ctx "resources/grass.png");
  Hashtbl.add textures 4 (Gfx.load_image ctx "resources/spikes.png");   
  Hashtbl.add textures 5 (Gfx.load_image ctx "resources/spikes.png");
  Hashtbl.add textures 6 (Gfx.load_image ctx "resources/spikes.png");   
  Hashtbl.add textures 7 (Gfx.load_image ctx "resources/ground.png");
  Hashtbl.add textures 8 (Gfx.load_image ctx "resources/platform.png");
  Hashtbl.add textures 9 (Gfx.load_image ctx "resources/flag_top.png");
  Hashtbl.add textures 14 (Gfx.load_image ctx "resources/flag_bottom.png");
  Hashtbl.add textures 15 (Gfx.load_image ctx "resources/lava.png");

  (* bgTextures est une hashtable contenant la ressource et un integer *)
  (* Ce integer représente de combien de pixels vertical on doit se décaler quand on dessine l'image comparé à l'ancienne *)
  (* Exemple: Texture 1 à 0, texture 2 à 10, texture 3 à 20. On dessinera la texture 2 en Y: 10 et la texture 3 en Y:30 *)
  Hashtbl.add bgTextures 10 ((Gfx.load_image ctx "resources/background_1/layer_0.png"), 0);
  Hashtbl.add bgTextures 11 ((Gfx.load_image ctx "resources/background_1/layer_1.png"), 210);
  Hashtbl.add bgTextures 12 ((Gfx.load_image ctx "resources/background_1/layer_2.png"), 50);
  Hashtbl.add bgTextures 13 ((Gfx.load_image ctx "resources/background_1/layer_3.png"), 60);

  Hashtbl.add bgTextures 20 ((Gfx.load_image ctx "resources/background_2/layer_0.png"), 0);
  Hashtbl.add bgTextures 21 ((Gfx.load_image ctx "resources/background_2/layer_1.png"), 205);
  Hashtbl.add bgTextures 22 ((Gfx.load_image ctx "resources/background_2/layer_2.png"), 80);
  Hashtbl.add bgTextures 23 ((Gfx.load_image ctx "resources/background_2/layer_3.png"), 50);
  Hashtbl.add bgTextures 24 ((Gfx.load_image ctx "resources/background_2/layer_4.png"), 0);

  (* 11 sprites de joueur, donc on charge les 11 images *)
  for i = 0 to 11 do
    Hashtbl.add textures (30 + i) (Gfx.load_image ctx ("resources/player/tile0" ^ (Printf.sprintf "%02d" i) ^ ".png"))
  done;

  let f = (fun key va -> Resource_queue.add (fun () -> Gfx.resource_ready va)) in

  Hashtbl.iter f textures;

  () ;;

let texture_from_type (b: t) =
  let blockType = b#block_type#get in

  match blockType with
    | Block_type.Player -> 1
    | Block_type.Spikes -> 4
    | Block_type.Ground -> 7
    | Block_type.Platform -> 8
    | Block_type.FlagTop -> 9
    | Block_type.FlagBottom -> 14
    | Block_type.Lava -> 15
    | _ -> 2;;

let white = Gfx.color 255 255 255 255;;
let maxTrace = 30;;
let playerTrace = Array.make maxTrace Vector.{x = 0.0; y = 0.0};;
let writeIndex = ref 0;;

let font_64 = Gfx.load_font "ForestDashFont" "" 64;;
let font = Gfx.load_font "ForestDashFont" "" 32;;
let font_24 = Gfx.load_font "ForestDashFont" "" 24;;
let font_18 = Gfx.load_font "ForestDashFont" "" 18;;
let playerAnim = ref 0;;

let drawOutlinedText ctx surf text font txtw txth gap clr clrBorder = 
  Gfx.set_color ctx clrBorder;

  Gfx.render_fasttext ctx text font txtw (txth+.gap);

  Gfx.set_color ctx clr;
  Gfx.render_fasttext ctx text font txtw txth;;

let drawBackground ctx win_surf (plypos: Vector.t) =
  (* Parallax, on déplace à une vitesse différente le background que le plan principal *)
  let alternative = Level_load.get_levelid() == 2 in
  let writeY = ref 0 in

  for i = 1 to (if alternative then 5 else 4) do
    (* On récupère la texture, et l'espace requis *)
    let (backgroundTexture, space) = Hashtbl.find bgTextures ((if (Level_load.get_levelid() != 2) then 10 else 20) + (i-1)) in
    writeY := !writeY + space;

    if Gfx.resource_ready backgroundTexture then begin
      let tex = (Gfx.get_resource backgroundTexture) in
      let contextSize = Gfx.surface_size tex in
      let width = (fst contextSize) in
      let widthf = (float_of_int width) in
      (* Ici, on trouve à quel position X on doit dessiner l'image, on prends la position X du joueur, auquel on applique un facteur
      qui réduit en fonction que le décor est lointain *)
      (* Rep est le nombre de répétition, car dans certains cas, on devra dessiner l'image une, ou deux, ou plus, fois *)
      let startBackgroundX = -((int_of_float (plypos.x/. (10.0 /. float_of_int(i)))) mod width) in
      let rep = int_of_float (Float.ceil ((1024.0 -. (float_of_int startBackgroundX)) /. widthf)) in

      for xIndex=0 to rep do
        Gfx.blit ctx win_surf tex (startBackgroundX + (((fst contextSize))*xIndex)) !writeY;
      done;
    end;
  done;;

let frame = ref 0;;

let update _dt el =
  frame := !frame + 1;
  let win = Game_state.get_window () in
  let ctx = Gfx.get_context win in
  let win_surf = Gfx.get_surface win in
  let w, h = Gfx.get_context_logical_size ctx in

  let () = Gfx.set_color ctx white in
  let () = Gfx.fill_rect ctx win_surf 0 0 w h in

  let ply = Game_state.get_player() in
  let plypos = ply#position#get in

  (* Position X de la caméra, TOUJOURS 200 pixels derrière le joueur *)
  let cameraX = int_of_float (plypos.x -. 200.0) in
  let (camW, camH) = Gfx.surface_size win_surf in

  (* On veut rafraichir le joueur seulement si il a sauté, on ne veut pas animer en cas de chute *)
  if ply#on_jump#get != 1 then
    ply#rot#set (ply#rot#get +. 9.0);

  (* Avant tout, on dessine le background *)
  drawBackground ctx win_surf plypos;
  let levelEnd = ref 1.0 in

  Seq.iter (fun (e : t) ->
    (* En dehors du cadre, on dessine pas*)
    let Vector.{ x; y } = e # position # get in
    let Rect.{width; height} = e # box # get in

    let relativeX = (int_of_float x) - cameraX in

    (* Le niveau fini à levelEnd, utile pour dessiner la progression après la boucle *)
    if e#block_type#get == Block_type.FlagBottom then
      levelEnd := x;

    (* Le point à gauche ne dépasse pas l'écran par la droite, et le point à droite est supérieur à 0/ Dépasse pas l'écran par la gauche*)
    if (relativeX < camW && relativeX + width > 0) then
      let texture = Hashtbl.find textures (texture_from_type e) in

      if (Gfx.resource_ready texture) then begin
        match e#block_type#get with
          | Player -> 
            (* 
            Curseur pour écrire dans la liste, concernant les particules qui suivent le joueur
            On pourrait tout simplement écrire à la fin de la liste et retirer le premier élément,
            mais cela demanderait de recréer une liste à chaque fois.
            Le curseur permet d'écrire à la place de la "donnée" la plus ancienne de la liste.
            *)
            writeIndex := !writeIndex + 1;
  
            if !writeIndex >= maxTrace then
              writeIndex := 0;

            (* On remplace la donnée la plus ancienne par la position du joueur *)
            playerTrace.(!writeIndex) <- e#position#get;
 
            (* On doit dessiner sa trace *)
            for i = 1 to (maxTrace-2) do
              (* Modulo car on ne part pas de l'index 0, mais d'un index compris dans [0; taille liste[
              Donc, on pourrait être à la fin de la liste ou autre *)
              let readIndex = (i + !writeIndex) mod maxTrace in
              let oldPos = playerTrace.(readIndex) in
              (* On réduis la taille en fonction de l'ancienneté de la donnée *)
              let size = float_of_int(i) /. 30.0 in
                    
              let distY = (y -. oldPos.y) in
              let distX = (x -. oldPos.x) in
              (* On force la seed aléatoire à être la même à chaque fois, pour avoir une consistance dans les couleurs choisis par le random, cela évite de stocker la couleur dans la liste *)
              Random.init(readIndex);
              let euclidDist = Random.int 10 in
                    
              (*let ang = ((Float.atan2 distY distX) *. 180.0 /. 3.141592) +. 180.0 in
              Gfx.set_transform ctx ang false false;*)

              begin
                if (readIndex mod 2) == 1 then
                  Gfx.set_color ctx (Gfx.color 32 132 199 (int_of_float (255.0 *. size)))
                else
                  Gfx.set_color ctx (Gfx.color 212 47 127 (int_of_float (255.0 *. size)));
              end;

              (* On dessine à l'ancienne position, moins le X de la caméra pour que cela soit dans le cadre, plus width/2 pour que cela parte de la position centrale du joueur *)
              (* EuclidDist car taille aléatoire de la particule, qu'on veut faire partir au centre vertical du joueur *)
              Gfx.fill_rect ctx win_surf (int_of_float oldPos.x - cameraX + width/2 - 2) (int_of_float oldPos.y + height/2 - euclidDist/2 - (Random.int 20 - 10) - 2) (euclidDist+2) (euclidDist+2);

              (*Gfx.reset_transform ctx;*)
            done;
                
            (* Player icone *)
            if e#rot#get != 0.0 then
              Gfx.set_transform ctx e#rot#get false false;
            
            (* Quel animation/Image du joueur on va jouer ? Car on a 12 sprites ( ou 11 ) *)
            (* + 30 car les images du joueur sont à l'index 30 dans la liste des textures *)
            let animID = (int_of_float (Float.floor (mod_float (_dt *. 2.0) 1200.0) /. 120.1)) + 30 in
            let animTex = Hashtbl.find textures animID in
                  
            if Gfx.resource_ready animTex then
              Gfx.blit_scale ctx win_surf (Gfx.get_resource animTex) relativeX (int_of_float y) width height;

            if e#rot#get != 0.0 then Gfx.reset_transform ctx;

          | _ -> 
            (* On applique une rotation si il y a besoin*)
            let is_heightSup = width < height in
            let minSize = if is_heightSup then width else height in
            let max = if is_heightSup then height else width in
                      
            let displayRatio = (float_of_int max) /. (float_of_int minSize) in
            let displayInt = int_of_float (Float.floor displayRatio) in
                        
            let tex = (Gfx.get_resource texture) in

            (* On affiche ce qu'on peut avant le reste, par exemple si height/width = 1.5. Alors on doit dessiner un bloc entier. Et 0.5 de bloc, c'est ça le reste *)
            try
              for i = 0 to displayInt-1 do
                (* L'effet que le terrain derrière le joueur disparaît *)
                let ratioBehind = float_of_int(if is_heightSup then relativeX else (relativeX + i * height)) in
                
                (* Min, pour que les blocs devant soit en 1.0 d'alpha, et que derrière, cela soit en 0.X
                Divisé par 100 car le joueur à 200 pixels horizontal derrière lui qui sont affichés entre lui et la partie gauche de l'écran
                Cela influencera donc les blocs derrière lui, entre 0 et 100 pixels horizontal dans l'écran *)
                let ratioBehind = min (ratioBehind /. 100.0) 1.0 in
                
                (* Vu qu'on le réduit, si on ajuste pas le X, alors il va se réduire vers le point en haut à gauche, mais on veut qu'il se réduise en son centre *)
                let bonusX = int_of_float(((1.0 -. ratioBehind) *. float_of_int(minSize)) /. 2.0) in
                
                (* Transparence, < 0.99 pour les problèmes de flottant *)
                Gfx.setGlobalAlpha ctx (if ratioBehind < 0.99 then ratioBehind/.2.0 else 1.0);

                let white = (Gfx.color 255 255 255 (int_of_float(ratioBehind *. 255.0))) in
                Gfx.set_color ctx white;
                
                (* Maintenant, on dessine le bloc
                Soit width < height, donc on dessine horizontalement les blocs, soit l'inverse *)
                
                if is_heightSup then begin
                  let drawValue = ((int_of_float y) + i * width) in

                  if (drawValue > 1024) then
                    (* Inutile de continuer la boucle, cela va ralentir le jeu dans le menu sinon *)
                    raise Exit
                  else if drawValue + minSize > 0 then
                    (* La formule en plus simple, sans prendre en compte les blocs derrière le joueur est:
                    positionX du bloc, positionY du sous-bloc ( vu qu'on dessine verticalement, on dessine plusieurs fois en Y différents, mais le X reste le même ), tailleduBloc, tailleduBloc *)
                    Gfx.blit_scale ctx win_surf tex (relativeX + bonusX) drawValue (int_of_float(float_of_int(minSize) *. ratioBehind)) (int_of_float(float_of_int(minSize) *. ratioBehind));
                  end
                else begin
                  let drawValue = (relativeX + i * height) in

                  if (drawValue > 1024) then
                    raise Exit
                  else if drawValue + minSize > 0 then
                    Gfx.blit_scale ctx win_surf tex (drawValue + bonusX) (int_of_float y + bonusX) (int_of_float(float_of_int(minSize) *. ratioBehind)) (int_of_float(float_of_int(minSize) *. ratioBehind));
                end;

                (* Réinitialise la transparence *)
                Gfx.setGlobalAlpha ctx 1.0;
              done;
            with
              | Exit -> ();

            let rest = int_of_float (fst (Float.modf displayRatio) *. (float_of_int minSize)) in

            if is_heightSup then
              Gfx.blit_scale ctx win_surf tex relativeX (int_of_float y + displayInt * width) width rest
            else
              Gfx.blit_scale ctx win_surf tex (relativeX + displayInt * height) (int_of_float y) rest height
          end
          ) el;

          (* A combien de pourcent est le joueur de la fin du niveau *)
          let ratio = Float.ceil ((min ((plypos.x +. (float_of_int (ply# box # get).width)) /. !levelEnd) 1.0) *. 100.0) in
          
          (* Dans le menu ? *)
          if (Level_load.get_levelid() != 0) then begin
            let tx = ((Printf.sprintf "%.1f" ratio)^"%") in
            let white = (Gfx.color 255 255 255 255) in
            
            Gfx.set_color ctx white;
            drawOutlinedText ctx win_surf tx font 100.0 55.0 2.0 white black;
            (*Gfx.blit ctx win_surf (Gfx.render_text ctx tx font) 100 55;*)
            
            Gfx.set_color ctx white;
            Gfx.fill_rect ctx win_surf 100 100 (int_of_float ratio*2) 4;
          end;

    if Level_load.get_levelid() == 0 then begin
      drawOutlinedText ctx win_surf "Controles" font_24 412.0 300.0 4.0 white black;
      drawOutlinedText ctx win_surf "C     Sauter" font_18 412.0 340.0 4.0 white black;
      drawOutlinedText ctx win_surf "1   Niveau 1" font_18 412.0 358.0 4.0 white black;
      drawOutlinedText ctx win_surf "2   Niveau 2" font_18 412.0 376.0 4.0 white black;

      (* Animation pour faire bouger le texte *)
      let t = Float.sin(_dt /. 400.0) in
      let ytitle = 125.0 +. (t *. 15.0) in

      drawOutlinedText ctx win_surf "Forest" font_64 330.0 ytitle 8.0 (Gfx.color 120 224 143 255) (Gfx.color 7 153 146 255);
      drawOutlinedText ctx win_surf "  Dash" font_64 330.0 (ytitle+.70.0) 8.0 (Gfx.color 249 151 119 255) (Gfx.color 111 27 27 255);
      let texture = Hashtbl.find textures 30 in
      Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) 360 (int_of_float (ytitle+.15.0)) 64 64;

    end;

    Gfx.commit ctx;;
