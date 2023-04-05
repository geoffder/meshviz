```ocaml
MaterialMap.set_value
  (get Material.maps MaterialMapIndex.(to_int Metalness) mat)
  1.0;
MaterialMap.set_value (get Material.maps MaterialMapIndex.(to_int Roughness) mat) 0.;
```

Setting these won't do anything since the lighting shader is not taking them
into account. See the raylib PBR model viewer for example of working with
these properties.
https://github.com/victorfisac/rPBR

It doesn't seem as simple as using this shader in place of lighting.fs (there
will be required setup added to lighting.ml, and setting of the relevant uniform
values before drawing the models. Also, might need to brdf shader stuff as well.
I'm not totally sure.)
https://github.com/victorfisac/rPBR/blob/master/release/resources/shaders/pbr.fs
https://github.com/victorfisac/rPBR/blob/master/src/pbrcore.h#L668