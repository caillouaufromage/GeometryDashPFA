let queueList = ref [||];;

let length() = Array.length (!queueList);;

let canStart() =
  let f a = a() in

  let res = Array.for_all f (!queueList) in
  res;;

let add (resourceFun: (unit -> bool)) =
  let deref = !queueList in
  queueList := Array.append deref [|resourceFun|];;