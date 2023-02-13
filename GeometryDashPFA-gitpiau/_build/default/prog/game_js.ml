let debug = Gfx.open_formatter "console"
let () = Gfx.set_debug_formatter debug
let () = Game.run ()