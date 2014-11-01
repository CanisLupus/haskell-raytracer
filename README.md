haskell-raytracer
=================

<h4>Description</h4>

A simple graphics ray tracer coded in Haskell and including a not-so-simple scene description file parser. You can use it as a learning tool or as guidance when creating a similar ray tracer :)

The ray tracer is compatible with the online course "CS184.1x Foundations of Computer Graphics" at edX, making use of the scene setup file format used there (file with the all data of the scene to be traced). I completed the course with a C++ ray tracer, but due to respect for the teachers of the course I'll not post that code, as C++ is the "official" course programming language. If you use this repository to help you with that course, DO learn from it and DON'T simply copy work.

<h4>Features</h4>

- Sphere, Triangle and Plane intersection routines (extendable to general convex polygons)
- Point and directional lights (as well as ambient light)
- Matrix transformations (translate, scale, rotate) for collisions in object space
- Precious (and horrible) code for inverting a 4x4 Matrix, converted from <a href="http://stackoverflow.com/questions/1148309/inverting-a-4x4-matrix">this post</a> (C++)
- Shadows and reflections
- Phong reflection model (plus emission value)
- Perspective camera with field-of-view
- Multi-threaded
- Support for scene description files (containing the scene setup), thanks to the massive help I got <a href="http://stackoverflow.com/questions/26315094/how-to-read-settings-and-geometric-shapes-from-a-file-in-haskell-for-later-use">here</a>.*

*Disclaimer: The final code for parsing the scene is very ugly. I'm not yet experienced enough to do this sort of task cleanly in Haskell.

<h4>Not-Features</h4>

The ray tracer does <string>not</strong> include refraction, soft shadows, cone lights, texture support, more complex surfaces (think cylinders and cones) or fancy global illumination techniques. It is also very simple regarding optimization. I took care in avoiding unnecessary computations, but didn't implement any optimizations that are essential for a fast ray tracer, such as bounding boxes or other partition schemes.

<h4>Haskell dependencies:</h4>

	cabal install bmp
	cabal install parsec-numbers
	cabal install vect

<h4>Compilation</h4>

	ghc -threaded -O2 --make raytracer.hs

<h4>Execution</h4>

	raytracer.exe +RTS -N

-N runs in as many CPU threads as you have, -N2 runs on 2 threads, -N4 on 4, etc.

<h4>Known issues</h4>

- The scene configuration files use PNG as the image output format, but this ray tracer removes ".png" from the file name and saves as BMP instead.
