open Raylib

let n_lights = ref 0
let max_lights = 4

module Light = struct
  type typ =
    [ `Directional
    | `Point
    ]

  let typ_to_int = function
    | `Directional -> 0
    | `Point -> 1

  type t =
    { mutable typ : typ
    ; mutable position : Vector3.t
    ; mutable target : Vector3.t
    ; mutable color : Color.t
    ; mutable enabled : bool
    ; loc_enabled : int
    ; loc_typ : int
    ; loc_pos : int
    ; loc_target : int
    ; loc_color : int
    }

  let update_typ t shader typ =
    let typ = Ctypes.allocate Ctypes.int (typ_to_int typ) in
    set_shader_value shader t.loc_typ (to_voidp typ) ShaderUniformDataType.Int

  let set_typ t shader typ =
    t.typ <- typ;
    update_typ t shader typ

  let update_position t shader p =
    set_shader_value shader t.loc_pos (to_voidp (addr p)) ShaderUniformDataType.Vec3

  let set_position t shader p =
    t.position <- Vector3.(create (x p) (y p) (z p));
    update_position t shader t.position

  let update_target t shader p =
    set_shader_value shader t.loc_target (to_voidp (addr p)) ShaderUniformDataType.Vec3

  let set_target t shader p =
    t.target <- Vector3.(create (x p) (y p) (z p));
    update_target t shader t.target

  let update_enabled t shader b =
    let enabled = Ctypes.allocate Ctypes.bool b in
    set_shader_value shader t.loc_enabled (to_voidp enabled) ShaderUniformDataType.Int

  let set_enabled t shader b =
    t.enabled <- b;
    update_enabled t shader b

  let update_color t shader c =
    let f i8 = Float.of_int i8 /. 255.0 in
    let clr = Color.(Vector4.create (f @@ r c) (f @@ g c) (f @@ b c) (f @@ a c)) in
    set_shader_value shader t.loc_color (to_voidp (addr clr)) ShaderUniformDataType.Vec4

  let set_color t shader c =
    t.color <- Color.(create (r c) (g c) (b c) (a c));
    update_color t shader t.color

  let make typ position target color shader =
    let enabled_name = Printf.sprintf "lights[%i].enabled" !n_lights in
    let typ_name = Printf.sprintf "lights[%i].type" !n_lights in
    let pos_name = Printf.sprintf "lights[%i].position" !n_lights in
    let target_name = Printf.sprintf "lights[%i].target" !n_lights in
    let color_name = Printf.sprintf "lights[%i].color" !n_lights in
    let t =
      { enabled = true
      ; typ
      ; position
      ; target
      ; color
      ; loc_enabled = get_shader_location shader enabled_name
      ; loc_typ = get_shader_location shader typ_name
      ; loc_pos = get_shader_location shader pos_name
      ; loc_target = get_shader_location shader target_name
      ; loc_color = get_shader_location shader color_name
      }
    in
    update_enabled t shader t.enabled;
    update_typ t shader t.typ;
    update_position t shader t.position;
    update_target t shader t.target;
    update_color t shader t.color;
    t

  let toggle t shader = set_enabled t shader (not t.enabled)
end

type t =
  { shader : Shader.t
  ; view_pos : int
  ; ambient : int
  ; lights : Light.t option array
  }

let load () =
  let site = List.hd DuneSites.Sites.shaders in
  let path n = Filename.concat site n in
  let shader = load_shader (path "lighting.vert") (path "lighting.frag") in
  if Shader.id shader = Unsigned.UInt.zero
  then failwith "Lighting shader failed to compile";
  let view_pos = get_shader_location shader "viewPos"
  and ambient = get_shader_location shader "ambient" in
  { shader; view_pos; ambient; lights = Array.make max_lights None }

let unload t = unload_shader t.shader
let shader t = t.shader

let set_ambient t v =
  set_shader_value t.shader t.ambient (to_voidp (addr v)) ShaderUniformDataType.Vec4

let set_view_pos t v =
  set_shader_value t.shader t.view_pos (to_voidp (addr v)) ShaderUniformDataType.Vec3

let light_count () = !n_lights

let create_light ?(typ = `Directional) ?(target = Vector3.zero ()) ~pos ~color t =
  if !n_lights > max_lights
  then failwith "Too many lights"
  else (
    let light = Light.make typ pos target color t.shader in
    t.lights.(!n_lights) <- Some light;
    n_lights := !n_lights + 1 )

let set_light_typ t i typ =
  Option.iter (fun l -> Light.set_typ l t.shader typ) t.lights.(i)

let set_light_position t i pos =
  Option.iter (fun l -> Light.set_position l t.shader pos) t.lights.(i)

let set_light_target t i pos =
  Option.iter (fun l -> Light.set_target l t.shader pos) t.lights.(i)

let set_light_color t i c =
  Option.iter (fun l -> Light.set_color l t.shader c) t.lights.(i)

let toggle_light t i = Option.iter (fun l -> Light.toggle l t.shader) t.lights.(i)
