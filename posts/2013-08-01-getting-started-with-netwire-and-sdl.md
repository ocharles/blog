----
title: "Getting Started with Netwire and SDL"
----

Game programming is often seen as a challenging problem in Haskell, and it
puzzled me for a long time too. It is a problem that seems to lend itself to
very imperative solutions and while this is of course doable in Haskell, it can
be become unweildly to manage. However, state is not the only difficulty. Even
in programming languages that do have mutable state, game programming is tricky
because there is a lot of code required to manage state transitions, deal with
interpolation and other gritty details that are not specific to the game you are
building.

Functional reactive programming is one possible programming paradigm that aims
to eliminate some of these problems. Pioneered by
[Conal Elliott, Paul Hudak](http://www.haskell.org/haskellwiki/Research_papers/Functional_reactive_programming)
and others in the late 90's, functional reactive programming builds itself on the
notions of behaviours and events. Behaviours are time varying values, and events
are aribtrarily complex conditions that can be used to influence behaviours.

There are a few different libraries available for functional reactive
programming in Haskell, and in this post we will focus on
[Netwire](http://hackage.haskell.org/package/netwire). The documentation
states:

> Netwire is a library for functional reactive programming, that is for
  time-varying values. It allows you to express various reactive systems
  elegantly and concisely by using an embedded domain-specific language.

For a long time I've had "learn Netwire" in my list of ideas for free time, and
I'm happy to say that while I'm still a long shot from having developed full
intuition with Netwire, I've made enough progress to get some mildly interesting
results. The source code for the three challenges in this post are all available
on [Github](https://github.com/ocharles/ocharles.org.uk--Getting-Started-with-Netwire-and-SDL). See the `README.md` file for instructions on how to build
and run the project (don't worry, it's only a `cabal configure && cabal
build` away).

## Automaton arrows

Before we begin looking at Netwire, it's worth covering a little bit of
background material. The fundamental building block in Netwire is the automaton
arrow, which is not nearly as scary as it sounds! While we will be using
Netwire's `Wire` type, it's useful to first consider what an automaton arrow
looks like:

```haskell
data Auto a b = Auto (a -> (b, Auto a b))
```

So an automaton arrow is simply a function that takes an `a` and produces a `b`,
along with a new automaton arrow. For example we could have a basic automaton
arrow that acts like a counter:

```haskell
counter :: Auto () Int
counter = go 0
  where go n = Auto $ \() -> (n + 1, go $ n + 1)
```

We can step the automaton by providing a little helper:

```haskell
step :: Auto a b -> a -> (b, Auto a b)
step (Auto f) a = f a
```

And so stepping through our `counter`, we might do:

```haskell
let (_, a1) = step counter ()
    (_, a2) = step a1 ()
    (n, _) = step a2 ()
in n
```

Which produces the value *3*, as we expect.

Netwire builds on top of these automaton arrows in few ways. The first
enhancement is simply the addition of a time delta - the difference between now
and when the automaton was last stepped. We will see further differences as we
progress through the article. Thus, for now we can think of Netwire as being:

```haskell
data Wire a b = Wire (TimeDelta -> a -> (b, Wire a b))
```

## Challenge 1: Basic Movement

Now that we have a rough idea of the underlying concept behind Netwire, we're ready
to begin working on our first application. Here's our first goal:

*Build an application that displays a square that moves with constant velocity
from left to right*.

We can solve this challenge with a single time varying value - the x-coordinate
of the square. This x-coordinate should increase over time. Thus we need to
implement:

```haskell
challenge1 :: Wire e m a Double
```

The important parameters to note are `a` and `Double`. The `a` parameter is the
input to our wire, which is completely polymorphic as our wire will not respond
to input. The wire will output `Double`s at every instance .Don't worry about
the `e` and `m` parameters for now, though we will understand what they are
later.

To solve this challenge, we will use the `integral_` wire. The `integral_` wire
*integrates* its input with respect to time, and adds some constant (the constant
of integration) to give us a definite integral:

```haskell
integral_ :: Double -> Wire e m Double Double
```

You may remember from calculus that integrating velocity gives us distance, so
all we need to do is supply `integral_` the constant velocity, which we can do
so by composing a constant wire (velocity) with `integral_`:

```haskell
challenge1 :: Monad m => Wire e m a Double
challenge1 = integral_ 0 . pure 20
```

I create the velocity wire with `pure 20` - this wire ignores its
input and always produces 20. Then I integrate this value, and supply 0 as the
initial position. The `Monad m` constraint is required by composition, and
may be a bit of a spoiler as to what that `m` is!

(I should note that you don't even need to write `pure 20`, as `Wire` is an
instance of `Num`. However, I find that when teaching this it's a little clearer
to use `pure 20`.)

### Rendering with SDL

Now that we have written our first `Wire`, it would be lovely to see the result!
Here's the final code for the first challenge:

```haskell
import Prelude hiding ((.), id)

import Control.Wire
import qualified Graphics.UI.SDL as SDL

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 200 200 32 [SDL.SWSurface]
  go screen clockSession challenge1

 where

  go screen s w = do
    (x, w', s') <- stepSession_ w s ()

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)

    SDL.flip screen
    go screen s' w'

challenge1 :: Monad m => Wire e m a Double
challenge1 = integral_ 0 . pure 20
```

`main` contains a few calls to get SDL running, and then we begin the main game
loop by calling `go`. `go` is passed `challenge1`, which is the wire we wish to
run, and is also passed a `clockSession`, which is a small helper that Netwire
provides to take care of calculating the clock delta between frames for you.

In `go`, we use `stepSession_` to step the whole simulation forwards based on the
delta that `clockSession` calculates. `stepSession_` produces the output of the
wire (a `Double`, as specified by `challenge1`), a new wire (remember `step`
from `Auto` above), and finally a new `Session`. We then clear the screen by
filling it white, and then render a square at the `x` coordinate produced by
`stepSession_`. Finally, we make a recursive call to `go`, and continue the
simulation. And that's it!

## Challenge 2: Responding to Input

Now that we have the ability to move our square, lets see if we can do this with
the arrow keys. So the challenge is:

*The square will rest with 0 velocity. User's can interact with the square by
changing its velocity to -20 if the left arrow is pressed, and +20 if the right
arrow is pressed.*

Before even implementing this, lets consider the type of our
game wire. To react to key presses, we need to know which keys have been
pressed. We can represent this via a set of keys that changes depending on which
keys are up or down. Thus we are aiming for a wire of the type:

```haskell
challenge2 :: Wire e m (Set SDL.Keysym) Double
```

We now have a few options to implement this. We could build a wire ourselves
that returns a velocity depending on the contents of `Set Key`. This single wire
would have to be built out of one large function which is not particularly
idiomatic Haskell - where we strive to build bigger programs out of small
programs and combinators. There is a  more idiomatic solution in Netwire, but to
get there we will need to understand the concepts of *event* and *wire inhibition*.

Earlier, I said that we could think of Netwire as being a lot like `Auto`.
However, `Auto` has a limitation in that it has to *always* produce a value. We
can lift this restriction, if we give the function the ability to not produce
output. This is called *inhibition*, and it provides the basics of events in
Netwire:

```haskell
data Wire e a b = Wire (Time -> a -> (Either e b, Wire e a b)
```

A `Wire` can now produce *either* a value of type `b`, or it can inhibit with
type `e`. This notion of inhibition allows us to build a system of events. If a
wire inhibits, then wires that are composed later will not be called - if one
part of a wire inhibits, the whole composition inhibits.

Events in Netwire are wires that act as the identity wire - the wire that passes
input through unmodified - if certain conditions are met. For example, the
`when` event is a `Wire` from `a` to `a`, but it only produces `a`s if the input
satifies a given boolean predicate. If not, the `Wire` inhibits, and no value is
produced until the predicate is satisfied.

Now that we have wires that can inhibit, we can begin to think of what it means
to run multiple wires. We simply try the first wire, and if it inhibits we try
the next wire, and so on. So we can now build a velocity wire that is one of three
alternatives:

- The user is pressing the "left arrow" key, so we produce -20
- The user is pressing the "right arrow" key, so we produce 20
- The user is not pressing either of these keys, so we produce 0

The Haskell translation is simply a formalisation of the above bullet points:

```haskell
challenge2_velocity  =  pure (-20) . when (keyDown SDL.SDLK_LEFT)
                    <|> pure 20 . when (keyDown SDL.SDLK_RIGHT)
                    <|> pure 0
```

Now we are able to write `challenge2`, which is only a minor variation on
`challenge1`:

```haskell
challenge2 :: (Monad m, Monoid e) => Wire e m (Set SDL.Keysym) Double
challenge2 = integral_ 0 . challenge2_velocity
```

### Using SDL for Input

We still need a way to provide input to this wire, and to do so we will use
SDL's event system. First, we have `parseEvents` which takes a `Set` of key
presses, and returns a modified `Set`:

```haskell
parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
  event <- SDL.pollEvent
  case event of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k -> parseEvents (insert k keysDown)
    SDL.KeyUp k -> parseEvents (delete k keysDown)
    _ -> parseEvents keysDown
```

Then I pass the empty set into the initial call to `go`, and `go` calls
`parseEvents` to update the set. The set is then passed to `stepSession_` and
passed back to `go` in the recursive call. `keyDown` is a little helper to check
the set of key presses for a key, ignoring modifiers:

```haskell
keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.symKey)
```

The full code for this challenge can be found [here](https://github.com/ocharles/ocharles.org.uk--Getting-Started-with-Netwire-and-SDL/blob/master/Challenge2.hs).  

## Challenge 3: Reacting to Collisions

Take a deep breath, because this challenge is going to be a step up from the
rest!

*Challenge 3: Allow the user to move a square around with the arrow keys. Arrow
key presses should apply acceleration to the square. If the square attempts to
move out of the bounds of the window, the velocity in that direction should be
negated, causing the square to bounce off the wall.*

Phew, quite a bit to do here! Firstly, lets briefly cover acceleration. To get
position from acceleration, we have to integrate twice - we integrate
acceleration to get velocity, and we integrate velocity to get position.
Unfortunately, we can't just use `integral_` anymore, as we need to encode more
logic.

The extra logic is required for both the velocity and the position wires. For the
velocity wire, we need to change the velocity depending on whether or not a
collision has occured. For the position wire, we need to clamp to the world
bounds.

Again, it's helpful to think by starting with the types, so lets sketch those
out:

```haskell
acceleration :: Wire e m (Set SDL.Keysym) Double
velocity :: Wire e m (Double, Bool) Double
position :: Wire e m Double (Double, Bool)
```

That got quite a bit more complicated! `acceleration` is now a `Wire` that
transform the set of key presses into an acceleration. `velocity` takes 
acceleration and a boolean representing whether a collision has occured, and
produces a velocity. Finally, `position` takes a velocity, and returns a pair of
position, and whether collisions occured - that is, whether the position had to be
adjusted to stay within the bounds of our world.

The `acceleration` wire is easy, that's just the same as `challenge2_velocity`
except we are repurposing it. Let's next move on to implementing the `velocity`
wire.

`integralLim_` is a function that Netwire provides us that is the same as `integral_`, except we
also get a post-update function. This post-update function takes three values as
input - the old and new values, and also some extra state that we can choose.
This post update function thus determines the new value of the integration, and
integration will continue from whatever value is returned (we can think of this
as giving us the ability to vary the constant of integration). Thus we can
either flip the velocity or leave it unchanged, depending on whether collisions
occured:

```haskell
velocity :: Wire e m (Double, Bool) Double
velocity = integralLim_ bounce 0
  where bounce collisions _ v | collisions = -v
                              | otherwise  = v
```

Now we only have to write `position`. `position` is again too much for
`integral_` to handle, but we also can't use `integralLim_` as we need to return
some extra information out of the wire (whether collisions occured), which
`integralLim_` cannot do. Thus we will implement `position` using `accumT`, which
is the underlying accumulator that the `integral` family of functions build on.

We accumulate starting at position 0 and assuming no collisions have happened.
At every instant, we move the square by the incoming velocity, scaled by the
amount of time that has elapsed. If this position falls outside the world
bounds, we adjust the square (with a small epsilon to stop it getting stuck in
the wall) and return the collision information:

```haskell
position :: Wire e m Double (Double, Bool)
position = accumT clamp (0, False)
  where clamp dt (x, _) v =
          let x' = x + dt * v
              coll = x < 0 || x > 150
              bounded = if coll then max 1 (min 149 x') else x'
          in (bounded, coll)
```

### ArrowLoop and Recursion

We have all the `Wire`s we need now, and the last piece of the puzzle is to put
them together. But there's a catch - lets look at the types of `velocity` and
`position` again:

```haskell
velocity :: Wire e m (Double, Bool) Double
position :: Wire e m Double (Double, Bool)
```

To run `velocity`, we have to supply `(Double, Bool)`, but to get the `(Double,
Bool)` tuple we have to run the `position` wire. And to run `position` we have to
run `velocity`, which needs `position`... uh oh!

The solution here is to use the `ArrowLoop` instance of `Wire`, which allows us
to create *recursive bindings*. `ArrowLoop` is far too magical for me to
understand and write by hand, but thankfully we have arrow notation in Haskell
to allow us to easily create loops. The final game wire is:

```haskell
challenge3 :: (MonadFix m, Monoid e) => Wire e m (Set SDL.Keysym) Double
challenge3 = proc keysDown -> do
  accel <- acceleration -< keysDown
  rec (position, collisions) <- position -< velocity
      velocity <- velocity -< (accel, collisions)
  returnA -< position
```

The complete listing for challenge 3 can be found [here](https://github.com/ocharles/ocharles.org.uk--Getting-Started-with-Netwire-and-SDL/blob/master/Challenge3.hs).

## Extra Challenges

More exercises that you might like to consider are:

* Moving vertically as well as horizontally. `Data.VectorSpace` (which Netwire
  depends on) makes this a breeze - it's mostly a case of changing acceleration,
  velocity and position to take a pair rather than a `Double`.

* Adding friction. Try and "absorb" some of the energy when collisions happen,
  or when no acceleration is applied.

* Limiting the velocity. Currently the velocity has no limit, but most things
  don't move with a limitless velocity!

## Concluding Thoughts

Learning Netwire has been a challenging experience, but quite possibly some of
the most rewarding learning I've done all year. I haven't experienced excitement
and amazement this strong since I originally began my programming hobby writing
games in BASIC many moons ago. I imagine this is partly because building
real-time interactive applications is very personal and imersive, but also
because it's been so challenging to get simple things done which has made me much
more appreciative when I get results.

But you didn't come here for me to get all nostalgic about the good ol' days. In
my opinion, Netwire (and FRP in general) embodies a fundamental shift in
thinking, and so far it's feeling like one that is a shift in the right direction.
That said, this stuff requires a lot of mental rewiring, and thus it can be
pretty tough at times. I can't say for sure if FRP is the future of game
programming, but it's certainly the immediate future for my hobby projects.

There's more to Netwire that I haven't even covered, too. We haven't looked at
actually using the inhibition value (the `e` parameter), nor have we made use of
the `Monad m` parameter. For example, you can use the latter to carry static
information across wires via a `Reader` monad which may be easier than plumbing
it through all the wires.

There are also a lot more prefabricated `Wire`s and `Wire` transformers - just
have a look at `Control.Wire.Prefab` and `Control.Wire.Trans` to get a taste of
what you get straight out of the box.

I highly recommend giving Netwire a try. The Haskell game programming community
is under-represented, and hopefully will encourage you to give it a shot - I
look forward to seeing what people come up with!
