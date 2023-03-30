(lang dune 3.7)

(name meshviz)

(generate_opam_files true)

(source
 (github geoffder/meshviz))

(authors "Geoff deRosenroll")

(maintainers "Geoff deRosenroll")

(documentation https://geoffder.github.io/meshviz)

(license GPL-3.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com")

(package
 (name meshviz)
 (synopsis "Mesh visualization tool")
 (description "Mesh visualization tool")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  (gg
   (>= 1.0.0))
  (OCADml
   (>= 0.4.0))
  raylib))
