oblicubes 0.1 (development)
===========================

New features
------------

* `oblicubesGrob()` / `grid.oblicubes()` renders cubes using a 3D
  oblique projection.  `oblicubesGrob()` returns a grid grob object while
  `grid.oblicubes()` also draws the grob to the graphic device.  As
  a special case may also render a 2D primary view orthographic projection.
  Inspired by [coolbutuseless](https://github.com/coolbutuseless)'s [{isocubes}](https://github.com/coolbutuseless/isocubes) package.
* `oblicuboidsGrob()` / `grid.oblicuboids()` renders cuboids using a 3D
  oblique projection.  `oblicuboidsGrob()` returns a grid grob object while
  `grid.oblicuboids()` also draws the grob to the graphic device.  As
  a special case may also render a 2D primary view orthographic projection.
  Inspired by [cj-holmes](https://github.com/cj-holmes)'s [{isocuboids}](https://github.com/cj-holmes/isocuboids) package.
* `xyz_heightmap()` calculate x,y,z coordinates (and optionally colors) from a height matrix.
