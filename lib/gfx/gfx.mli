exception GfxError of string 
(** All errors are reported with this exception.
    The payload is an informative message. *)

type window
(** type of windows.
    - JavaScript : represents the initial canvas element
    - SDL : represent a top-level window *)

type surface
(** type of surfaces. These are rectangules of pixels onto which
    one can draw, blit, etc…
*)

type context
(**
    type of rendering context.
*)

type color
(** type of colors *)

type font
(** font objects *)

type 'a resource
(** remote resource *)

val resource_ready : 'a resource -> bool
(** tests if the resource is available. *)

val get_resource : 'a resource -> 'a
(** returns the content of the resource.
    raises Failure with an appropriate message if the resource is not ready. 
        *)

val backend : string
(** [backend] if the name of the backend. It can be ["sdl"] or
    ["js"].
*)

val create : string -> window

(** [create s] returns a window and a rendering surface from the string [s].
    The string has the form ["name:WxH:flags"].
    For the JavaScript backend, [name] is element id of the canvas
    representing the window. In the SDL backend, [name] is the window
    title. *)

val get_window_size : window -> int * int
(** returns the dimensions in screen pixels of the window
  *)

val set_window_size : window -> int -> int -> unit
(** sets the dimensions in pixels of the window *)

val get_context : window -> context
(** [get_context w] : returns the context of window [w] *)


val set_context_logical_size : context -> int -> int -> unit
(** [set_context_logical_size ctx w h] sets the logical size of the context.
    The initial values are the same dimentions as the window. The logical size
    reflects the range of pixels that are shown in the context. For instance,
    If the logical size is 100x100 but the window size is 400x400, each logical pixel
    will be automatically zoomed and displayed as a 4x4 pixel in the window.    
*)

val setGlobalAlpha : context -> float -> unit
val get_context_logical_size : context -> int * int
(** [get_context_logical_size ctx w h] returns the logical size of the context.  
*)

val set_transform : context -> float -> bool -> bool -> unit 
(** [set_transform ctx angle hflip vflip] stores a transformation in the context. The
    transformation is a rotation of [angle] (in radians), on horizontal
   reflection (if [hflip] is [true]) and a vertical reflection (if [vflip]) is
    [true]). *)


val get_transform : context -> float * bool * bool
(** [get_transform ctx] returns the transformation currently associated with the context. 
    *)

val reset_transform : context -> unit
(* [reset_transform ctx] is an alias for [set_transform ctx 0.0 false false]. *)

val get_surface : window -> surface
(** [get_surface w] : returns the underlying surface of window [w] *)

val create_surface : context -> int -> int -> surface
(**
    [create_surface ctx w h] : creates a surface for the given rendering
    context.
*)

type audio
val load_sound : string -> bool -> audio resource
val play_sound : audio -> unit
val stop_sound : audio -> unit

val surface_size : surface -> int * int
(** returns the dimensions of a surface *)

val blit : context -> surface -> surface -> int -> int -> unit
(** [blit dst src x y]
    copies surface [src] on surface [dst] at point [(x,y)] *)

val blit_scale :
  context -> surface -> surface -> int -> int -> int -> int -> unit
(** [blit_scale ctx dst src dx dy dw dh] copies surface [src] on surface [dst]
      at point [(dx,dy)] scaling it to [dw] width and [dh] height *)

val blit_full :
  context ->
  surface ->
  surface ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  unit
(** [blit_full ctx dst src sx sy sw sh dx dy dw dh]
    copies the surface extracted from [src] at point [(sx, sy)] with dimensions [(sw, sh)]
    on surface [dst] at point [(dx,dy)] scaling it to [dw] width and [dh] height *)

val color : int -> int -> int -> int -> color
(** [color r g b a] returns a color built from components red green blue and
      alpha. all values must be integers between 0 and 255 inclusive. *)

val set_color : context -> color -> unit

val fill_rect : context -> surface -> int -> int -> int -> int -> unit
(** [fill_rect ctx dst x y w h c] draws and fills a rectangle on surface surface
      [dst] at coordinates [(x, y)] and with dimensions [w * h]. The rectangle is
      filled with color c *)

val load_image : context -> string -> surface resource
(** [load_image ctx path] loads an image whose content is
    given by an implementation dependent string (filename, url, … ).
    Common image types are supported (PNG, JPEG, …).
    The returned resource may not be extracted used until [resource_ready] returns [true].
    *)

val load_font : string -> string -> int -> font
(**
    [load_font fn extra size] loads font [fn] at size [size] given in points.
    The [extra] parameters allows to pass implementation dependent options.
    - JavaScript : [fn] is a font name. If it does not exist, it is silently replaced
    by a close matching font or default font by the browser.
    - SDL : [fn] must be a path to the [.ttf] file containing the font. 
  *)

val render_text : context -> string -> font -> surface
(** [render_text ctx txt f c] returns a surface containing the text
    [txt] rendered using font [f] with color [c].
*)

val render_fasttext : context -> string -> font -> float -> float -> unit

val measure_text : string -> font -> int * int
(** [mesure_text  txt f] returns the size (width height) of the surface that
      [render_text] would return, without creating it.
*)

type event =
  | NoEvent (** no event *)
  | KeyUp of string (** Key with a given name was released *)
  | KeyDown of string (** Key with a given name was pressed *)
  | Mouse

  (** The type of input events. 
      The string describing keyboard events is implementation defined.
*)

val poll_event : unit -> event
(** [poll_event ()] returns the next event in the event queue *)

val main_loop : (float -> bool) -> unit
(** [main_loop f] calls a [f] repeteadly but no faster than 60 times/seconds.
    The callback [f] is given a float representing the elapsed time in
    milliseconds since the begining of the program. It should return true to
    continue being called or false to stop the [main_loop]
*)

val commit : context -> unit
(** [commit ctx]  : renders the rendering context on to its underlying window.
    This should be the last graphical operation of the current frame.
    *)

val load_file : string -> string resource
(** [load_file path] creates a resource that, when ready, resolves to
    the content of the file denoted by path.
    - SDL : path is the path of a file.
    - JS : path is a URL to be loaded with  *)

val open_formatter : string -> Format.formatter
(** [open_formatter src] opens a formatter for debugging purposes.
    - SDL : [src] is the file path.
    - JavaScript : [src] must be the ID of an element whose innerHTML is appended to.
    It is recommended to style the element with the CSS property white-space:pre
    to preserve white spaces.
    *)

val set_debug_formatter : Format.formatter -> unit
(** [set_debug_formatter fmt] sets the current formatter.
    The default value is Format.stderr.
    - SDL : writing to Format.stderr writes to the standard error as usual.
    - JavaScript : writing to Format.stderr writes to the JavaScript console.
      in error mode (console.error).
*)

val debug : ('a, Format.formatter, unit) format -> 'a
(** [debug f a b c d...] prints to the currently configured debug formatter.
    The first argument [f] is a format string (see {!Format.printf}). *)