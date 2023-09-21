open Component_defs

let level = ref 0;;
let get_levelid () =
  !level;;
  
let levelBlockList = ref [];;
  
let set_level (id : int) =
  if(id > 2) then
    level := 0
  else
    level := id;;