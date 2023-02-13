open Ecs


class position =
  object
    val position = Component.def Vector.zero
    method position = position
  end

class color =
  object
    val color = Component.def (Gfx.color 0 0 0 255)
    method color = color
  end

class box =
  object
    val box = Component.def Rect.{ width = 0; height = 0}
    method box = box
  end

(* Question 2 *)
class mass =
  object
    val mass = Component.def 0.0
    method mass = mass
  end

class velocity =
  object
    val velocity = Component.def Vector.zero
    method velocity = velocity
  end

class sum_forces =
  object
    val sum_forces = Component.def Vector.zero
    method sum_forces = sum_forces
  end

class rot =
  object
    val rot = Component.def 0.0
    method rot = rot
  end

class on_jump =
  object
    val on_jump = Component.def 2
    method on_jump = on_jump
  end

class block_type =
  object
    val block_type = Component.def Block_type.Solid
    method block_type = block_type
  end

class inverted_gravity =
  object
    val inverted_gravity = Component.def true
    method inverted_gravity = inverted_gravity
  end

class type player =
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
    inherit rot
    inherit inverted_gravity
  end
