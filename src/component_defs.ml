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

class on_ground =
  object
    val on_ground = Component.def false
    method on_ground = on_ground
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
    inherit on_ground
  end