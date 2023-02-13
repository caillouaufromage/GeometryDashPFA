open Vector

type t = { width : int; height : int }


let mdiff v1 b1 v2 b2 =
  (* We use the Minkowski difference of Box1 and Box2 *)
  let x = v1.x -. v2.x -. float b2.width in
  let y = v1.y -. v2.y -. float b2.height in
  let h = b1.height + b2.height in
  let w = b1.width + b2.width in
  ({ x; y }, { width = w; height = h })

let has_origin v b =
  v.x <= 0.0
  && v.x +. float b.width >= 0.0
  && v.y <= 0.0
  && v.y +. float b.height >= 0.0
