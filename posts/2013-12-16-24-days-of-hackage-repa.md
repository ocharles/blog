---
title: 24 Days of Hackage: repa
---

Today we're going to step into the high performance realm of Hackage, and take a
look at Ben Lippmeier's
[`repa`](http://hackage.haskell.org/package/repa). `repa` is a library for high
performance multi-dimensional array computations, where computations are built
up in a very natural, compositional manner. `repa` offers a few different
*evaluation strategies*, which enable it to perform computations on very large
arrays either sequentially or in parallel. Alongside that, there's a lot of
fusion magic going on behind the scenes, which can give blazingly fast
performance.

`repa` employs some moderately advanced type tricks to keep track of the state
of the array, which can be a little confusing at first. Let's start by disecting
the most fundamental type - the array type:

```haskell
data Array r sh e
```

Arrays in `repa` are parameterized over 3 types: `r`, the representation of the
array; `sh`, the shape of the array; and `e`, the element type in the
array. `e` is the most obvious - if you want an array of integers, then you
would have `Array r sh Int`, while an array of characters would be
`Array r sh Char`, and so on. Next, let's consider the `sh` parameter.

The shape of an array is its dimensions, but in `repa` these dimensions form
part of the type. This means the *type* of a two dimensional array is different
to the type of a three dimensional array. Having different types mean we get
type level checks that our computations make sense. For example, in the general
case it doesn't make sense to zip together arrays of different dimensions, and
if you do attempt to do this GHC will reject your program and refuse to compile
it.

The final type parameter to consider is `r`, which describes the array
representation. The representation type describes to `repa` the state of the
array. To enumerate a few options, there are delayed arrays (which are like lazy
values), unboxed arrays, bytestring arrays, and more. You generally won't need
to concern yourself with this for most `repa` programming, but you may well come
across requirements on the representation type from time to time.

## Image Manipulation with Repa

I've been going through my photo collection recently, and I can't help but feel
that everything is little... lacking... for the current festive season. It would
be great if I could write something that would take my boring photos and make
them much more seasonal! I'm thinking the addition of a few snowflakes ought to
do the job. Today, we'll build a little application that uses `repa` to
superimpose a few snowflakes on top of an image.

To get started, we need a way to load in an image as `repa` array. We'll use
`JuicyPixels` to do the raw IO, and then we'll pluck pixels out into a `repa`
array:

```haskell
loadImage :: FilePath -> IO (Array D DIM3 Word8)
loadImage path = do
  Right (JP.ImageRGBA8 img) <- JP.readImage path
  return $ fromFunction
    (Z :. imageHeight img :. imageWidth img :. 4)
    (\(Z :. y :. x :. c) -> case JP.pixelAt img x y of
                              JP.PixelRGBA8 r g b a ->
                                case c of
                                  0 -> r
                                  1 -> g
                                  2 -> b
                                  3 -> a)
```

We simply load the image with `JuicyPixels` (aliased as `JP`) and then use
`repa`'s `fromFunction` constructor to build a array. The `D` in the type
signiture indicates that this array is delayed, and not yet evaluated.

Now that we know we have a way to load images, let's considered how to blend
images together. We'll need a function that takes two `repa` arrays and combines
them together. We'll also take an offset for the snowflake. We're looking to
implement a function like:

```haskell
addSnowflake
  :: (Source r1 Word8, Source r2 Word8)
  => Array r1 DIM3 Word8
  -> (Int, Int)
  -> Array r2 DIM3 Word8
  -> Array D DIM3 Word8
addSnowflake snowflake (offsetX, offsetY) source =
```

We need to walk over two arrays to build a new one, so we'll use `traverse2` to
do this:

```haskell
addSnowflake snowflake (offsetX, offsetY) source =
  traverse2 source snowflake resize blend
```

Along with the two arrays to traverse, `traverse2` needs a function to compute
new elements, and a function to determine the new size of the array. The new
size is easy - just take the size of the source array.

```haskell
  resize sourceSize _ = sourceSize
```

For computing each new pixel though, we need to do a bit more work. Each pixel
has four dimensions - the red, green, blue and alpha channels. For the new alpha
channel, we'll just take the original alpha channel. For the red, green and blue
channels, we'll linearly interpolate between the snowflake and the source image,
depending on the snowflake's alpha channel. This comes out quite succinctly,
with:

```haskell
  blend lookupSource lookupSnowflake p@(Z :. y :. x :. 3) =
    lookupSource p

  blend lookupSource lookupSnowflake p@(Z :. y :. x :. chan) =
    let (snowflakeX, snowflakeY) = (x - offsetX, y - offsetY)
        sourcePos = (Z :. snowflakeY :. snowflakeX :. chan)
        alpha = fromIntegral (lookupSnowflake (Z :. snowflakeY :. snowflakeX :. 3)) / 255
 
    in if inShape (extent snowflake) sourcePos
         then let a = fromIntegral (lookupSource p)
                  b = fromIntegral (lookupSnowflake sourcePos)
              in round $ a + (b - a) * alpha
         else lookupSource p
```

With a little bit of plumbing in `main :: IO ()`, we can turn these boring
images...

<div style="text-align: center">
<img src="/img/2013-12-16-ocharles.png" style="width: 400px;" />
</div>

Into these much more cheery ones!

<div style="text-align: center">
<img src="/img/2013-12-16-festive-ocharles.png" style="width: 400px;" />
</div>

As always, there's a lot we didn't cover in today's post. User `SirRockALot1` mentions the following:

> You didn't touch on what is probably to me the most interesting feature about
> `repa`, its [stencil support](http://en.wikipedia.org/wiki/Stencil_codes). I was
> originally introduced to `repa` because I wanted to implement the
> standard/naive Game of Life grid algorithm with it, and I saw this beautiful
> implementation using repa stencils:
> http://www.tapdancinggoats.com/haskell-life-repa.htm

Not only do we have the `repa` library, there's also a collection of other
libraries that work with `repa` in Hackage, including:

* [`repa-io`](http://hackage.haskell.org/package/repa-io) to load arrays from
  disk
* [`repa-algorithms`](http://hackage.haskell.org/package/repa-algorithms)
  provides some common algorithms on `repa` arrays
* [`repa-devil`](http://hackage.haskell.org/package/repa-devil) integrates
  `repa` with the [DevIL](http://openil.sourceforge.net/) image library to load
  images.

You can find today's code on [Github](http://github.com/ocharles/blog) - go have
a play!
