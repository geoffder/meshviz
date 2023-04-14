open Raylib

type t

val load : unit -> t
val unload : t -> unit
val update : t -> Camera.t -> Vector2.t -> unit
val set_render_mode : t -> int -> unit
val set_view_pos : t -> Vector3.t -> unit
val pbr_shader : t -> Shader.t
val skybox_shader : t -> Shader.t
val max_lights : int
val light_count : unit -> int

val create_light
  :  ?typ:[ `Directional | `Point ]
  -> ?target:Vector3.t
  -> pos:Vector3.t
  -> color:Color.t
  -> t
  -> unit

val set_light_position : t -> int -> Vector3.t -> unit

val draw_model_pbr
  :  ?pos:Vector3.t
  -> ?rot_ax:Vector3.t
  -> ?rot:float
  -> ?scale_:Vector3.t
  -> t
  -> Model.t
  -> unit
