type playState = Menu | Playing;;

val get_window : unit -> Gfx.window
val set_window : Gfx.window -> unit
val set_player : Component_defs.player -> unit
val get_player : unit -> Component_defs.player
val set_playstate : playState -> unit
val get_playstate : unit -> playState