-----
title: "Asteroids & Netwire"
-----

Since my initial foray into functional reactive programming using Netwire, I
decided to challenge my newly acquired knowledge by attempting a larger project.
I eventually decided to build a clone of the classic Atari game -
[Asteroids](https://en.wikipedia.org/wiki/Asteroids_%28arcade_game%29). I'm happy
to announce that I've now reached a point at which I'm happy to call this
complete, and want to share my findings with you in this blog post. Before we go
into that, here's a look at the finished product:

<iframe width="420" height="315" src="//www.youtube.com/embed/0A63Wc4xLIc" frameborder="0" allowfullscreen></iframe>

## Why Asteroids?

I chose Asteroids as it felt like a complete game that would require solving a
few distinct challenges. Firstly, Asteroids features different types of objects - the
player's ship, the asteroids themselves, bullets, and UFOs. Each of these
objects have slightly different behaviours - both on their own and when
interacting with other objects. For example, the player ship can be interacted
with by the player, but also destroyed by asteroids and UFOs.

Asteroids is also one of the simplest dynamic worlds I can think of. The amount
of objects on screen varies with time, and each object has an independent life
time. Independent objects with variable lifetimes are an integral part of
almost any game now, so I decided to start simple and see if I could uncover any
elementary approaches to this.

Another important aspect of any game is that of varying state. The game
itself varies in state depending on the number of asteroids on screen. When the
count reaches zero, the game starts again with a higher number of asteroids and
the score roles over. This management of a state at a much granular level than
frame by frame motion felt like it would be an area worth exploring.

Finally, Asteroids felt like an achievable goal for a single person still
learning technology. The rendering is extremely simplistic, as are the physics
themselves. Because of this, I was able to go a little further than planned, and
ended up building a small sound engine and particle system.

## Proud Moments

Along with finishing the game itself, there are a few smaller parts of the
project that I feel especially proud of.

### A Functional Reactive Sound Engine

As I approached the end of the project, I felt like it wasn't a true Asteroids
clone unless I had some lo-fi sounds to accompany gameplay. Feeling confident
with Netwire, I decided it would be fun to synthesize sounds myself with code
rather than using prerecorded sounds. As such, I developed a tiny little
synthesizer using Netwire

As an example, here's how I generate explosion sounds:

```haskell
explosion = decay 2 . gate . (rateReduce &&& 0.4) .  (quantize &&& 500) .
              (noise &&& 0.2)
```

So my explosion sound is random noise, quantized to 20%, sampled at 500hZ, and
gated at 40%. This random noise decays linearly over 2 seconds to 0. After 2
seconds, this wire inhibits. I then use a little function to "render" a `Wire`
to an SDL `Chunk`, and am able to play the sound whenever I need to.

This has made synthesizing sounds a really fun (and especially geeky!)
experience. The sound DSL could be improved; one notable problem is that
currently it's a little strange that parameters aren't quite paired up with the
function they are a parameter too. For example, to apply 20% quantization, one
must write:

```haskell
quantize . (noise &&& 0.2)
```

Which may read more like 0.2 being a parameter to `noise`, not `quantize`. Arrow
notation may help here, but I went for a one-liner than did the job. However,
some sounds are expressed elegantly without arrow notation due to `Wire`s being
instances of the `Num` type class:

```haskell
ufo = for (1 / 3) . sin . ((sin . 3) * 200 + 100)
```

`sin` is a `Wire` taking frequency as input, and producing an amplitude between
-1 and 1. In this sound, I take a 3hZ sine wave, amplify it 200 times and
offset that by 100Hz. I then feed this into another sine oscillator, in order to
produce a pitch that flucuates to produce a cheesy UFO sound. The overloading of
the `Num` type class lets me treat the amplification and offset stages as normal
arithmetic, which I find particularly elegant.

### Particle System

Another bit of polish to the game was adding a rudimentary particle system to
use for asteroid explosions. The particle system itself is just a few lines of
code:

```haskell
particleSystems
  :: (Applicative m, MonadRandom m) => Wire e m [V2 Double] [V2 Double]
particleSystems = go []
 where
  go systems = mkGen $ \dt newSystemLocations -> do
    stepped <- mapM (\w -> stepWire w dt ()) systems

    let alive = [ (r, w) | (Right r, w) <- stepped ]
    spawned <- concat <$> mapM spawnParticles newSystemLocations

    return (Right (map fst alive), go $ map snd alive ++ spawned)

  spawnParticles at = do
    n <- getRandomR (4, 8)
    replicateM n $ do
      velocity <- randomVelocity (5, 10)
      life <- getRandomR (1, 3)
      return ((for life <!> ()). integrateVector at . pure velocity)
```

This wire takes as its input, a list of locations to create new particle
systems. As output, it produces a list of all particles. Each particle system
has a random amount of particles, each alive for a random duration and moving
with a random velocity from the particle systems origin. I wrote this code
essentially in a single attempt, and the output was just what I was looking for.

## Expressive Events

The event system in Netwire takes a while to get your head around, but it can
ultimately lead to succinct expression of events. The `isShooting` event is one
of my favourite's here, and it's defined as:

```haskell
isShooting =
  asSoonAs (keyDown SDL.SDLK_SPACE) >>> (once --> coolDown >>> isShooting)
 where
  coolDown =
    arr head . multicast [ after 0.05, asSoonAs (not . keyDown SDL.SDLK_SPACE) ]
```

This event reads very naturally. The player is shooting as long as the space key
is held down, at which point they shoot once, and then enter a cool down period.
I use `multicast` to combine two events as you would combine booleans under
conjunction with `&&` - inhibiting until both 0.05s have elapsed and the player
has released the space key during or after this interval. Once both of these
criteria are met, the event repeats. This leads to shooting happening at most
once every 0.05s, and requires the player to toggle the space bar - as I find this
type of interaction pleasing for this type of game (as it feels more energetic and
increases tension).

Another similar example of chaining events is to determine when a UFO should
spawn:

```haskell
ufoSpawned = (once --> ufoSpawned) . wackelkontaktM (1 / 1000) . after 30
```

`wackelkontaktM` is an awfully named wire, but it's essentially a wire that only
produces with a given probability. Thus a UFO spawns once after 30 seconds, and
then with a probability of 1/1000. After a UFO spawns, we repeat the event,
waiting at least 30 seconds.

I will admit that these events do take a little bit of trial and error to get
the behaviour you want. Usually, you'll find that wires can be composed in
various different orders, and the composition is not necessarily equal.

For example, we might assume that `ufoSpawned` could be rewriten as:

```haskell
ufoSpawned = once . wackelkontaktM (1 / 1000) . after 30 --> ufoSpawned
```

But this will never spawn UFOs, and has significantly different semantics,
despite reading very similarly. Perhaps this is a drawback of very expressive
DSLs!

## Difficulties

### Understanding ArrowLoop Is Important

In my previous post about Netwire, I said:

> ArrowLoop is far too magical for me to understand [..], but thankfully we have
> arrow notation in Haskell to allow us to easily create loops.

This is true, but it only lets you go so far. As I developed my clone, I needed
to use recursive bindings in order to implement collisions. At that time, I
didn't understand how `ArrowLoop` really worked, and naively assuming that it
wolud Do the Right Thing. The result of such a naive approach can be summarized
with one word and a few symbols that any Haskell programmer would dread to see:
`<<loop>>` - the computation will never produce a value as it forms an infinite
loop.

My original approach attempted to use definitions relative to the current frame
in a mutually recursive way: a frame's collisions depended on the positions
of objects alive in the frame, but the notion of life depended on the current
frame's collisions. As you can see - there is no way to produce an answer if we
phrase equations in this form.

The solution is to understand that ArrowLoop needs a base case, just as
recursion does, in order to produce an answer. I finally solved my problem by
operating on the alive entities from the *previous* frame (using the `delay`
`Wire`), and start with a base case of the empty list on the first frame.

Unfortunately, nothing in the types will prevent you from making such
nonsensical bindings. I wonder if there is a possibility to encode this
information in the type system, and [Productive Programming Using Guarded
Recursion](http://bentnib.org/productive.pdf) immediately comes to mind - though
that paper is still beyond me so it may not apply. Nonetheless, I'm convinced
there may be at least something that can be done.

### Space Leaks

I encountered only a single space leak in my clone, which is extremely
promising. At one stage of development, I needed to improve the rendering of
asteroids from being simple circles to polygons. I decided that it would be
easiest to express asteroids as a list of vector's from the origin of varying
magnitude, and I originally codified this as a behavior such that:

> spikes = keep . randomSpikes

The `keep` `Wire` holds the first value produced by `randomSpikes` and always
produces it thereupon. However, a composition of this form will still do some
work on every frame (even if that work is just building up thunks) up until the
`keep` `Wire` is actually reached. In my case I ended up building up so many
`randomSpike` invocations I eventually exhausted the stack. I didn't spend time
working out why this would cause such a drastic space leak and not get garbage
collected (that may be a bug in the `keep` `Wire`, or it may be in my usage).
Eitherway, naively combining wires like this can bite you - so it's worth seeing
if things can be expressed in a different way. In the end, I chose to improve
the `randomSpikes` wire to produce a single result, and then switch it's
behavior to be a constant wire.

### Managing Wire Collections

It took me a while to formulate a way to express collections of wires that can
grow and shrink. For example, when the player is shooting, a new bullet needs to
join the world and interact with other objects, but should also be able to
destroy itself as bullets have a finite lifetime.

In the end, I ended up pairing behaviour values at specific instants with the
`Wire` that will produce the next instant value. Thus this is almost as if I am
reifying wires themselves. By doing this, I am now able to build a list of
`Wire`s present in the *next* frame based on the *current* values.

I make use of this technique with the main collision `Wire`. `collision` takes
as its input a list of bullet positions, paired with the wire to produce the
position of the bullet at the next frame, and likewise for UFOs and asteroids.
`collision` then filters these lists by taking various Cartesian products and
determine what is colliding at what isn't. The final result is a list of active
bullets, active asteroids, destroyed asteroids, and so on.

I can step a collection of these `Wire`s with the following:

```haskell
stepWires :: Monad m => Wire e m [Wire e m () b] [(b, Wire e m () b)]
stepWires = mkFixM $ \dt objects -> do
  stepped <- mapM (\o -> stepWire o dt ()) objects
  return $ Right [ (o, w') | (Right o, w') <- stepped ]
```

As we can see, this takes a `Wire` collection as input, and produces a list of
behaviours along with their next `Wire`. I'm not convinced this is necessarily
the best way to do this, but it feels like a more functional approach when
compared to other alternatives such as inventing unique identifiers for each
object and storing this all in a `Map`.


## Other Difficulties

The major difficulties I had in implementing this game were inherent to the game
itself, and so while my experience was certainly not bug free, I'm really happy
that the majority of problems were due to bad assumptions about the game logic
itself.

For example, I started with bullets being spawned at the centre of the ship
itself, moving with a fixed velocity determined by the player's rotation. Once I
added player-bullet collisions due to UFO's firing, the player would immediately
explode whenever they shot as their own bullet would collide with themselves for
a single frame. Offsetting the bullet is the easiest fix here.

There are similar bugs due to bad reasoning, such as incosistent vector/matrix
multiplication, but again these were not due to choice of technology. Perhaps these
too could be further reduced by leveraging the type system, but currently I'm
quite happy with the current pragmatic balance between correctness and
programmer productivity.


## Conclusion

I thoroughly enjoyed building this game using Haskell and Netwire - to the point
where I became a little bit addicted to the project. The more I learn about
Netwire, the more fluently I'm able to express certain behaviours very
naturally. Having state encapsulated in each wire independently is an extremely
powerful technique for managing state, and for the most part has meant that I
don't have to think about state at all.

Netwire seems to almost require a certain discipline in programming - but
thankfully it's a discipline that we should all follow as much as possible
anyway. That discipline is short, focused, functions. Everytime I tried to
create complicated wires I ran into problems, and as soon as I started to break
things apart into smaller wires and compose them together I could once again
reason about my program and progress to the desired result.

The event model for Netwire can make it quite simple to add instanteous events,
though again it also requires a little bit of upfront thinking to decide which
type of composition is appropriate.

Hopefully this post should convince people that game programming is certainly
possible. More importantly, game programming in Haskell is possible and it
*doesn't* require threading everything through state monads or otherwise writing
imperative code. Netwire does require a little bit of thinking about how you are
going to phrase certain relationships, but I found that this thinking was at a
very abstract level in order to determine who depends on whom - rather than fine
grained implementation details. This is the type of thinking that I *enjoy*
doing, and expect to do as a programmer. It's fantastic that Netwire lets me
focus on this, allowing me to separate it from the rest of the programming.

The full code to my clone can be found at
[on Github](http://github.com/ocharles/netwire-classics/). If you want to build
it, you'll need `lens`, `linear`, `netwire`, `SDL`, and `SDL-mixer` from
Hackage. I may get round to writing a cabal file for this at some point!
