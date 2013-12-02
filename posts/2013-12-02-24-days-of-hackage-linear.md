---
title: 24 Days of Hackage: linear
---

Anyone who wants to do game programming at some point ends up writing (parts of)
their own linear algebra library - only to realize how painful hard it is to
actually get it right. This point doesn't just crop up in game programming
either - linear algebra is useful for all sorts of tasks, including the rise of
[machine learning](http://stackoverflow.com/questions/5943611/linear-algebra-application-in-machine-learning). While
linear algebra libraries seem to be a perfect example of a type hierarchy, it
quickly becomes clear that getting a good abstraction is very
difficult. Thankfully, you don't have to worry about that - because Edward Kmett
has already done it for you.

A quick disclaimer: for the first part of this post, we'll be exploring the
library itself, and I'll assume you know some of the basic parts of linear
algebra. Familiarity with vectors, matrices, and so on will help. Towards the
end of the post we'll do some practical stuff with this - so some readers may
wish to [skip to the end](#Using_linear).

[`linear`](http://hackage.haskell.org/package/linear) is a set of "types and
combinators for linear algebra on free vector spaces." Well, it wouldn't be
Edward's library if it didn't have "co-" or "free" in the description would it?
Lets take a look at some of the modules we've got available to us:

* `Linear.Affine` gives us access to affine spaces - which are loosely described
  as vector spaces that have "forgotten" their origin. For the rest of us, this
  just gives us a type class to find the difference between two vectors, and
  also a convenient `Point` new type so that we can be clear that we are working
  with points in a space, rather than vectors.

* `Linear.Matrix` allows us to define m×n matrices, and calculate matrix sums,
  products, and so on. Specifically, `Linear.Matrix` works at a very general
  level to work on matrices of any size, but also leverages the type system to
  guarantee that only well-formed matrix products can be calculated.

* `Linear.Metric` provides functions to compute metrics on algebraic
  structures. I won't go into what is meant by "metric", though
  [Jeremy Kun](http://jeremykun.com/) has written
  [about these before](http://jeremykun.com/2011/12/19/metrics-on-words/). Here
  you will find dot products, norms and normalization routines.

* `Linear.Quaternion` covers the basic construction and manipulation of
  [*quaternions*](http://mathworld.wolfram.com/Quaternion.html), which are often
  used to define rotations, at least in the use of linear algebra for 3D
  geometry.

* `Linear.Vector` contains the type classes for working with
  vectors. Specifically we have `Additive`, which gives us access to vector
  difference, sums and linear interpolation between vectors. On top of this, we
  have left/right scalar products, basis vectors, and a few other bits and
  pieces.

* If `Linear.Vector` defines the abstract machinery to work with vector, the
  `Linear.V` family of modules gives us concrete instantiations of vectors
  themselves. Specifically `Linear.V0` to `V4` give us 0 to 4 dimensional
  vectors, while `Linear.V` lets us define vectors of any dimension using
  singleton types to carry a witness of the vector's dimension.

Already, `linear` appears to be a formidable library for anyone needing to work
with linear algebra, but for me a big part of the joy of working with `linear`
is how far reaching it is. While `linear` comes with a collection of
hand-crafted types, the library also has the ability to work with a lot of types
defined outside `linear`. For example, `IntMap` can model vectors, so it's
perfectly valid to compute the `norm` of an `IntMap`:

```haskell
> norm (IntMap.fromList $ map (id &&& fromIntegral) [0..10])
19.621416870348583
```

Or the dot product of two `IntMap`s:

```haskell
> dot (IntMap.fromList [(0, 5), (1, 0)])
      (IntMap.fromList [(0, 1), (1, 1)])
5
```

This is very powerful when we move away from the 3 or 4 dimensional vectors
found in game programming, and need to use linear algebra to solve problems in
machine learning.

Of more interest (at least to me) is the ability of `linear` to interact with
the world outside of Haskell - such as using `linear`'s types in OpenGL
programs. OpenGL is a low level API for programming graphics cards and rendering
3D graphics, and being low-level OpenGL mostly works in terms of bits and
bytes. Fortunately we have a type class for transforming Haskell values to bits
and bytes in Haskell - the `Storable` type class. `linear`'s vector and matrix
types are indeed instances of `Storable`, so we can easily communicate with
OpenGL. Sounds like a great place to look at some code!

## <a name="Using_linear"></a>Programming OpenGL with `linear`

We'll build a tiny little OpenGL program to spin and translate an OpenGL
triangle, à la demo scene - albeit a very boring demo scene!. To do this, we're
going to need to calculate a few different things:

* We will need a perspective projection matrix to have the illusion of
  depth. Creating a perspective projection matrix is fairly formulaic in terms
  of the value in each row-column entry of the matrix. We'll use the `vector`
  library to generate this.

* We will also need a matrix defining the affine transformation to apply to our
  triangle. Specifically, this transformation is going to be the composition of
  two smaller transformations - a translation and a rotation.

Let's begin with these smaller matrices:

```haskell
triangleRotation :: (Epsilon a, Floating a) => a -> M44 a
triangleRotation t =
  m33_to_m44 $
    fromQuaternion $
      axisAngle (V3 0 1 0) (t * 2)

triangleTranslation :: Floating a => a -> M44 a
triangleTranslation t =
  eye4 & translation .~ V3 (sin t * 2) 0 (-5)
```

Both our transformations are varying over time, so they take `t` as a
parameter. For our rotation we use a quaternion around the yaw axis. This
quaternion is then converted to a matrix - specifically a 3×3 matrix, which we
then convert to a 4×4 matrix. The translation matrix is defined by taking a 4×4
identity matrix, and then using the  `translation` lens to set the translation
component of this transformation to be -5 units along the z-axis (away from the
virtual camera), and `sin t * 2` units along the x-axis.

Finally, we compose these matrices to get our final transformation:

```haskell
triangleTransformation :: (Epsilon a, Floating a) => a -> M44 a
triangleTransformation =
  liftA2 (!*!) triangleTranslation triangleRotation
```

(I'm using the `(Double -> a)` applicative functor to compose these
transformations, mostly for fun/unnecessary code golf).

Now we have a 4×4 transformation matrix, but unfortunately the rows and columns
are in the wrong order for OpenGL (at least by default). We need to transpose
our matrix - but there doesn't seem to be a transposition function in `linear`!
And that's correct, and by design - because transposition generalizes to far
more than just matrices. What we're looking for is a function of the type:

```haskell
transpose :: outer (inner a) -> inner (outer a)
```

It turns out there is a whole class of functors that have this behavior - the
class of *distributive functors*. These are captured in the
[`distributive`](http://hackage.haskell.org/package/distributive) library. Thus
to communicate our matrix with OpenGL, we `distribute` the matrix first, and
then we use the `Storable` instance to turn our matrix into bits and bytes:

```haskell
with (distribute $ triangleTransformation t) $ \ptr ->
  GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))
```

We need to give Haskell a hint to the underlying type of the matrix to take a
pointer to - in this case we use the OpenGL `CFloat` type. The end result looks
like this:

<div style="text-align: center; margin: 1em 0">
<iframe width="420" height="315" src="//www.youtube.com/embed/119xvvQ13LU" frameborder="0" allowfullscreen></iframe>
</div>

You can find the code I used to generate this over in the `code/` folder in this
blog's
[Github repository](https://github.com/ocharles/blog/tree/master/code). It
requires the currently unreleased [SDL 2](http://github.com/lemmih/hsSDL2)
bindings I'm collaborating on, so it may be a bit tricky to build - sorry!
