open Raylib
open Tgl4
open Utils

let n_lights = ref 0
let max_lights = 4
let max_mipmap_levels = 5

(* TODO: for fields that are updated when the corresponding values are sent to
    the shader (so they can be checked), I should probably just make them
    mutable, since there isn't really a time when I want to pass them around
    like they are immutable structs anyway. n_lights could also be a mutable
    field in the type t I guess (there should only be one of it, perhaps a check
    to see if it is currently loaded/initialized (private global ref) and raise
    if load is called inappropriately). *)
module Light = struct
  type typ =
    [ `Directional
    | `Point
    ]

  let typ_to_int = function
    | `Directional -> 0
    | `Point -> 1

  type t =
    { typ : typ
    ; position : Vector3.t
    ; target : Vector3.t
    ; color : Color.t
    ; enabled : bool
    ; loc_enabled : int
    ; loc_typ : int
    ; loc_pos : int
    ; loc_target : int
    ; loc_color : int
    }

  let update_values shader light =
    let enabled = Ctypes.allocate Ctypes.bool light.enabled in
    set_shader_value shader light.loc_enabled (to_voidp enabled) ShaderUniformDataType.Int;
    let typ = Ctypes.allocate Ctypes.int (typ_to_int light.typ) in
    set_shader_value shader light.loc_typ (to_voidp typ) ShaderUniformDataType.Int;
    set_shader_value
      shader
      light.loc_pos
      (to_voidp (addr light.position))
      ShaderUniformDataType.Vec3;
    set_shader_value
      shader
      light.loc_target
      (to_voidp (addr light.target))
      ShaderUniformDataType.Vec3;
    let vec4_int f a b c d = Vector4.create (f a) (f b) (f c) (f d) in
    let color =
      vec4_int
        (fun accessor -> (accessor light.color |> float) /. 255.0)
        Color.r
        Color.g
        Color.b
        Color.a
    in
    set_shader_value
      shader
      light.loc_color
      (to_voidp (addr color))
      ShaderUniformDataType.Vec4

  let make typ position target color shader =
    let enabled_name = Printf.sprintf "lights[%i].enabled" !n_lights in
    let typ_name = Printf.sprintf "lights[%i].type" !n_lights in
    let pos_name = Printf.sprintf "lights[%i].position" !n_lights in
    let target_name = Printf.sprintf "lights[%i].target" !n_lights in
    let color_name = Printf.sprintf "lights[%i].color" !n_lights in
    let light =
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
    update_values shader light;
    light

  let set_position t shader p =
    Vector3.set_x t.position (Vector3.x p);
    Vector3.set_y t.position (Vector3.y p);
    Vector3.set_z t.position (Vector3.z p);
    set_shader_value
      shader
      t.loc_pos
      (to_voidp (addr t.position))
      ShaderUniformDataType.Vec3
end

module Pbr = struct
  type property =
    { mutable bitmap : Texture2D.t option
    ; mutable color : Color.t
    ; bitmap_loc : int
    ; use_bitmap_loc : int
    ; color_loc : int
    }

  type material =
    { albedo : property
    ; normals : property
    ; metalness : property
    ; roughness : property
    ; ao : property
    ; emission : property
    ; height : property
    }

  type t =
    { shader : Shader.t
    ; material : material
    ; view_pos_loc : int
    ; render_mode_loc : int
    ; model_matrix_loc : int
    }

  let setup_property shader name texture_unit color =
    let bitmap_loc = get_shader_location shader (name ^ "sampler") in
    let tu = CArray.(to_voidp (start @@ of_list Ctypes.int [ texture_unit ])) in
    set_shader_value_v shader bitmap_loc tu ShaderUniformDataType.Int 1;
    { bitmap = None
    ; color
    ; bitmap_loc
    ; use_bitmap_loc = get_shader_location shader (name ^ "useSampler")
    ; color_loc = get_shader_location shader (name ^ "color")
    }

  let setup_material shader color metalness roughness =
    { albedo = setup_property shader "albedo" 3 color
    ; normals = setup_property shader "normals" 4 (Color.create 128 128 255 255)
    ; metalness = setup_property shader "metalness" 5 (Color.create metalness 0 0 0)
    ; roughness = setup_property shader "roughness" 6 (Color.create roughness 0 0 0)
    ; ao = setup_property shader "ao" 7 (Color.create 255 255 255 255)
    ; emission = setup_property shader "emission" 8 (Color.create 0 0 0 0)
    ; height = setup_property shader "height" 9 (Color.create 0 0 0 0)
    }

  let load ?(color = Color.white) ?(metalness = 255) ?(roughness = 255) () =
    let shader = load_shader "pbr.vert" "pbr.frag" in
    let material = setup_material shader color metalness roughness
    and view_pos_loc = get_shader_location shader "viewPos"
    and render_mode_loc = get_shader_location shader "renderMode"
    and model_matrix_loc = get_shader_location shader "mMatrix" in
    (* setup texture units *)
    setup_constant_vals shader "irradianceMap" 0;
    setup_constant_vals shader "prefilterMap" 1;
    setup_constant_vals shader "brdfLUT" 2;
    { shader; material; view_pos_loc; render_mode_loc; model_matrix_loc }

  let unload t = unload_shader t.shader
  let shader t = t.shader
  let set_albedo_texture t tex = t.material.albedo.bitmap <- Some tex

  let set_render_mode { shader; render_mode_loc; _ } i =
    let mode = Ctypes.allocate Ctypes.int i in
    set_shader_value shader render_mode_loc (to_voidp mode) ShaderUniformDataType.Int

  let set_view_pos { shader; view_pos_loc; _ } v =
    set_shader_value shader view_pos_loc (to_voidp (addr v)) ShaderUniformDataType.Vec3
end

module Skybox = struct
  type t =
    { shader : Shader.t
    ; tex : Texture2D.t
    ; view_pos_loc : int
    ; resolution_loc : int
    ; projection_loc : int
    }

  let load () =
    let shader = load_shader "pbr.vert" "pbr.frag"
    and tex = load_texture (texture_path "pinetree.hdr") in
    let view_pos_loc = get_shader_location shader "view"
    and resolution_loc = get_shader_location shader "resolution"
    and projection_loc = get_shader_location shader "projection" in
    setup_constant_vals shader "environmentMap" 0;
    { shader; tex; view_pos_loc; resolution_loc; projection_loc }

  let unload t =
    unload_texture t.tex;
    unload_shader t.shader

  let shader t = t.shader

  let set_resolution { shader; resolution_loc; _ } v =
    set_shader_value shader resolution_loc (to_voidp (addr v)) ShaderUniformDataType.Vec2

  let set_projection { shader; projection_loc; _ } proj =
    set_shader_value_matrix shader projection_loc proj
end

type t =
  { pbr : Pbr.t
  ; skybox : Skybox.t
  ; cubemap_id : Gl.uint32_bigarray
  ; irradiance_id : Gl.uint32_bigarray
  ; prefilter_id : Gl.uint32_bigarray
  ; brdf_id : Gl.uint32_bigarray
  ; lights : Light.t option array
  }

(* let setup_material_pbr t albedo metalness roughness = () *)

let load () =
  let pbr = Pbr.load ()
  and skybox = Skybox.load () in
  (* ignore (skybox.resolution_loc, skybox.view_pos_loc); *)
  (* shaders *)
  let cubemap_shader = load_shader "cubemap.vert" "cubemap.frag"
  and irradiance_shader = load_shader "skybox.vert" "irradiance.frag"
  and prefilter_shader = load_shader "skybox.vert" "prefilter.frag"
  and brdf_shader = load_shader "brdf.vert" "brdf.frag" in
  (* temp constants (arguments to LoadEnvironment) *)
  let cubemap_size = 1024
  and irradiance_size = 32
  and prefilter_size = 256
  and brdf_size = 512 in
  (* locations *)
  let cubemap_projection = get_shader_location cubemap_shader "projection"
  and cubemap_view = get_shader_location cubemap_shader "view"
  and cubemap_id = create_uint32_bigarray 1
  and irradiance_projection = get_shader_location irradiance_shader "projection"
  and irradiance_view = get_shader_location irradiance_shader "view"
  and irradiance_id = create_uint32_bigarray 1
  and prefilter_projection = get_shader_location prefilter_shader "projection"
  and prefilter_view = get_shader_location prefilter_shader "view"
  and prefilter_roughness = get_shader_location prefilter_shader "view"
  and prefilter_id = create_uint32_bigarray 1
  and brdf_id = create_uint32_bigarray 1 in
  (* setup constants *)
  setup_constant_vals cubemap_shader "equirectangularMap" 0;
  setup_constant_vals irradiance_shader "environmentMap" 0;
  setup_constant_vals prefilter_shader "environmentMap" 0;
  (* setup depth face culling and cube map seamless *)
  Gl.(enable texture_cube_map_seamless);
  Gl.(depth_func lequal);
  Gl.(disable cull_face_mode);
  Gl.line_width 2.;
  (* setup framebuffer for skybox *)
  let capture_fbo = create_uint32_bigarray 1
  and capture_rbo = create_uint32_bigarray 1 in
  Gl.gen_framebuffers 1 capture_fbo;
  Gl.gen_renderbuffers 1 capture_rbo;
  Gl.(bind_framebuffer framebuffer (uint32_bigarray_get capture_fbo 0));
  Gl.(bind_renderbuffer renderbuffer (uint32_bigarray_get capture_rbo 0));
  Gl.(renderbuffer_storage renderbuffer depth_component24 cubemap_size cubemap_size);
  Gl.(
    framebuffer_renderbuffer
      framebuffer
      depth_attachment
      renderbuffer
      (uint32_bigarray_get capture_rbo 0) );
  (* setup cubemap to render and attach to framebuffer *)
  (* NOTE: faces are stored with 16bit floating point values *)
  Gl.(gen_textures 1 cubemap_id);
  Gl.(bind_texture texture_cube_map (uint32_bigarray_get capture_fbo 0));
  for i = 0 to 5 do
    let null = `Data (create_uint32_bigarray 0)
    and target = Gl.texture_cube_map_positive_x + i in
    Gl.(tex_image2d target 0 rgb16f cubemap_size cubemap_size 0 rgb float null)
  done;
  Gl.(tex_parameteri texture_cube_map texture_wrap_s clamp_to_edge);
  Gl.(tex_parameteri texture_cube_map texture_wrap_t clamp_to_edge);
  Gl.(tex_parameteri texture_cube_map texture_wrap_r clamp_to_edge);
  Gl.(tex_parameteri texture_cube_map texture_min_filter linear);
  Gl.(tex_parameteri texture_cube_map texture_mag_filter linear);
  (* create projection (transposed) and different views for each face *)
  let capture_projection = Matrix.transpose @@ Matrix.perspective 90. 1. 0.01 1000. in
  let capture_views =
    let v x y z = Vector3.create x y z in
    [| Matrix.look_at (v 0. 0. 0.) (v 1. 0. 0.) (v 0. (-1.) 0.)
     ; Matrix.look_at (v 0. 0. 0.) (v (-1.) 0. 0.) (v 0. (-1.) 0.)
     ; Matrix.look_at (v 0. 0. 0.) (v 0. 1. 0.) (v 0. 0. 1.)
     ; Matrix.look_at (v 0. 0. 0.) (v 0. (-1.) 0.) (v 0. 0. (-1.))
     ; Matrix.look_at (v 0. 0. 0.) (v 0. 0. 1.) (v 0. (-1.) 0.)
     ; Matrix.look_at (v 0. 0. 0.) (v 0. 0. (-1.)) (v 0. (-1.) 0.)
    |]
  in
  (* convert HDR equirectangular environment map to cubemap equivalent *)
  Gl.use_program (Unsigned.UInt.to_int @@ Shader.id cubemap_shader);
  Gl.(active_texture texture0);
  Gl.(bind_texture texture_2d (Unsigned.UInt.to_int @@ Texture2D.id skybox.tex));
  set_shader_value_matrix cubemap_shader cubemap_projection capture_projection;
  (* NOTE: don't forget to configure the viewport to the capture dimensions *)
  Gl.(viewport 0 0 cubemap_size cubemap_size);
  Gl.(bind_framebuffer framebuffer (uint32_bigarray_get capture_fbo 0));
  for i = 0 to 5 do
    let target = Gl.texture_cube_map_positive_x + i
    and id = Int32.to_int @@ Bigarray.Array1.get cubemap_id 0 in
    set_shader_value_matrix cubemap_shader cubemap_view capture_views.(i);
    Gl.(framebuffer_texture2d framebuffer color_attachment0 target id 0);
    Gl.(clear (color_buffer_bit lor depth_buffer_bit));
    RenderCube.render ()
  done;
  (* unbind framebuffer and textures *)
  Gl.(bind_framebuffer framebuffer 0);
  (* create an irradiance cubemap, and re-scale capture FBO to irradiance scale *)
  Gl.(gen_textures 1 irradiance_id);
  Gl.(bind_texture texture_cube_map (uint32_bigarray_get irradiance_id 0));
  for i = 0 to 5 do
    let null = `Data (create_uint32_bigarray 0)
    and target = Gl.texture_cube_map_positive_x + i in
    Gl.(tex_image2d target 0 rgb16f irradiance_size irradiance_size 0 rgb float null)
  done;
  Gl.(tex_parameteri texture_cube_map texture_wrap_s clamp_to_edge);
  Gl.(tex_parameteri texture_cube_map texture_wrap_t clamp_to_edge);
  Gl.(tex_parameteri texture_cube_map texture_wrap_r clamp_to_edge);
  Gl.(tex_parameteri texture_cube_map texture_min_filter linear);
  Gl.(tex_parameteri texture_cube_map texture_mag_filter linear);
  Gl.(bind_framebuffer framebuffer (uint32_bigarray_get capture_fbo 0));
  Gl.(bind_renderbuffer renderbuffer (uint32_bigarray_get capture_rbo 0));
  Gl.(renderbuffer_storage renderbuffer depth_component24 irradiance_size irradiance_size);
  (* solve diffuse integral by convolution to creat and irradiance cubemap *)
  Gl.(use_program (Unsigned.UInt.to_int @@ Shader.id irradiance_shader));
  Gl.(active_texture texture0);
  Gl.(bind_texture texture_cube_map (uint32_bigarray_get cubemap_id 0));
  set_shader_value_matrix irradiance_shader irradiance_projection capture_projection;
  (* NOTE: don't forget to configure the viewport to the capture dimensions *)
  Gl.(viewport 0 0 irradiance_size irradiance_size);
  Gl.(bind_framebuffer framebuffer (uint32_bigarray_get capture_fbo 0));
  for i = 0 to 5 do
    let target = Gl.texture_cube_map_positive_x + i
    and id = uint32_bigarray_get irradiance_id 0 in
    set_shader_value_matrix irradiance_shader irradiance_view capture_views.(i);
    Gl.(framebuffer_texture2d framebuffer color_attachment0 target id 0);
    Gl.(clear (color_buffer_bit lor depth_buffer_bit));
    RenderCube.render ()
  done;
  (* unbind framebuffer and textures *)
  Gl.(bind_framebuffer framebuffer 0);
  (* create a prefiltered HDR environment map *)
  Gl.(use_program (Unsigned.UInt.to_int @@ Shader.id prefilter_shader));
  Gl.(active_texture texture0);
  Gl.(bind_texture texture_cube_map (uint32_bigarray_get cubemap_id 0));
  set_shader_value_matrix prefilter_shader prefilter_projection capture_projection;
  Gl.(bind_framebuffer framebuffer (uint32_bigarray_get capture_fbo 0));
  for mip = 0 to max_mipmap_levels - 1 do
    let mip_wh = Float.(to_int @@ (of_int prefilter_size *. (0.5 ** of_int mip)))
    and roughness = Float.(of_int mip /. of_int (max_mipmap_levels - 1)) in
    Gl.(bind_renderbuffer renderbuffer (uint32_bigarray_get capture_rbo 0));
    Gl.(renderbuffer_storage renderbuffer depth_component24 mip_wh mip_wh);
    Gl.(viewport 0 0 mip_wh mip_wh);
    Gl.(uniform1f prefilter_roughness roughness);
    for i = 0 to 5 do
      let target = Gl.texture_cube_map_positive_x + i
      and id = uint32_bigarray_get prefilter_id 0 in
      set_shader_value_matrix prefilter_shader prefilter_view capture_views.(i);
      Gl.(framebuffer_texture2d framebuffer color_attachment0 target id mip);
      Gl.(clear (color_buffer_bit lor depth_buffer_bit));
      RenderCube.render ()
    done
  done;
  (* unbind framebuffer and textures *)
  Gl.(bind_framebuffer framebuffer 0);
  (* generate BRDF convolution texture *)
  Gl.(gen_textures 1 brdf_id);
  Gl.(bind_texture texture_2d (uint32_bigarray_get brdf_id 0));
  Gl.(tex_image2d texture_2d 0 rgb16f brdf_size brdf_size 0 rg float (`Offset 0));
  Gl.(tex_parameteri texture_2d texture_wrap_s clamp_to_edge);
  Gl.(tex_parameteri texture_2d texture_wrap_t clamp_to_edge);
  Gl.(tex_parameteri texture_2d texture_min_filter linear);
  Gl.(tex_parameteri texture_2d texture_mag_filter linear);
  (* render BRDF LUT into a quad using default FBO *)
  Gl.(bind_framebuffer framebuffer (uint32_bigarray_get capture_fbo 0));
  Gl.(bind_renderbuffer renderbuffer (uint32_bigarray_get capture_rbo 0));
  Gl.(renderbuffer_storage renderbuffer depth_component24 brdf_size brdf_size);
  let id = uint32_bigarray_get brdf_id 0 in
  Gl.(framebuffer_texture2d framebuffer color_attachment0 texture_2d id 0);
  Gl.(viewport 0 0 brdf_size brdf_size);
  Gl.use_program (Unsigned.UInt.to_int @@ Shader.id brdf_shader);
  Gl.(clear (color_buffer_bit lor depth_buffer_bit));
  RenderQuad.render ();
  (* unbind framebuffer and textures *)
  Gl.(bind_framebuffer framebuffer 0);
  (* then before rendering, configure the viewport to the actual screen dimensions *)
  let default_proj =
    let ratio = Float.(of_int (get_screen_width ()) /. of_int (get_screen_height ())) in
    Matrix.transpose @@ Matrix.perspective 60. ratio 0.01 1000.
  in
  set_shader_value_matrix cubemap_shader cubemap_projection default_proj;
  Skybox.set_projection skybox default_proj;
  set_shader_value_matrix irradiance_shader irradiance_projection default_proj;
  set_shader_value_matrix prefilter_shader prefilter_projection default_proj;
  (* reset viewport dimensions to default *)
  Gl.(viewport 0 0 (get_screen_width ()) (get_screen_height ()));
  (* unload shaders not kept in environment *)
  unload_shader cubemap_shader;
  unload_shader irradiance_shader;
  unload_shader prefilter_shader;
  unload_shader brdf_shader;
  { pbr
  ; skybox
  ; cubemap_id
  ; irradiance_id
  ; prefilter_id
  ; brdf_id
  ; lights = Array.make max_lights None
  }

let pbr_shader t = Pbr.shader t.pbr
let skybox_shader t = Skybox.shader t.skybox

let update t camera resolution =
  let cpos = Camera.position camera in
  let pos = Vector3.(create (x cpos) (y cpos) (z cpos))
  and res = Vector2.(create (x resolution) (y resolution)) in
  Pbr.set_view_pos t.pbr pos;
  Skybox.set_resolution t.skybox res

let unload t =
  (* FIXME: just ignoring because of unused warning for now *)
  ignore t.pbr.model_matrix_loc;
  (* unload shaders *)
  Pbr.unload t.pbr;
  Skybox.unload t.skybox;
  (* unload dynamic textures created in environment initialization *)
  Gl.(delete_textures 1 t.cubemap_id);
  Gl.(delete_textures 1 t.irradiance_id);
  Gl.(delete_textures 1 t.prefilter_id);
  Gl.(delete_textures 1 t.brdf_id)

let light_count () = !n_lights

let create_light ?(typ = `Directional) ?(target = Vector3.zero ()) ~pos ~color t =
  if !n_lights > max_lights
  then failwith "Too many lights"
  else (
    let light = Light.make typ pos target color t.pbr.shader in
    t.lights.(!n_lights) <- Some light;
    n_lights := !n_lights + 1 )

let set_light_position t i pos =
  Option.iter (fun l -> Light.set_position l t.pbr.shader pos) t.lights.(i)

let set_render_mode t i = Pbr.set_render_mode t.pbr i
let set_view_pos t p = Pbr.set_view_pos t.pbr p
