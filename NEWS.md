oblicubes 0.1.2
===============

Initial features
----------------

* `oblicubesGrob()` / `grid.oblicubes()` renders cubes using a 3D
  oblique projection.  `oblicubesGrob()` returns a grid grob object while
  `grid.oblicubes()` also draws the grob to the graphic device.  As
  a special case may also render a 2D primary view orthographic projection.
  Inspired by [coolbutuseless](https://github.com/coolbutuseless)'s [{isocubes}](https://github.com/coolbutuseless/isocubes) package.
* `geom_oblicubes()` provides a `{ggplot2}` geom wrapper to `oblicubesGrob()`.
* `oblicuboidsGrob()` / `grid.oblicuboids()` renders cuboids using a 3D
  oblique projection.  `oblicuboidsGrob()` returns a grid grob object while
  `grid.oblicuboids()` also draws the grob to the graphic device.  As
  a special case may also render a 2D primary view orthographic projection.
  Inspired by [cj-holmes](https://github.com/cj-holmes)'s [{isocuboids}](https://github.com/cj-holmes/isocuboids) package.
* `geom_oblicuboids()` provides a `{ggplot2}` geom wrapper to `oblicuboidsGrob()`.
* `xyz_heightmap()` calculate x,y,z coordinates (and optionally colors) from a height matrix.
  An adaption of `coords_heightmap()` from [coolbutuseless](https://github.com/coolbutuseless)'s [{isocubes}](https://github.com/coolbutuseless/isocubes).
* `darken_face()` is a function to darken the color of cube/cuboid faces.
  Meant to serve as the `light` argument of `oblicubesGrob()`, `grid.oblicubes()`, `oblicuboidsGrob()`, `grid.oblicuboids()`.
* `cheap_darken()` is a function to darken colors by a certain "amount".
  Faster (but less "good") then `colorspace::darken()`.
  A copy of an internal function from [coolbutuseless](https://github.com/coolbutuseless)'s [{isocubes}](https://github.com/coolbutuseless/isocubes).

