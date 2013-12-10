---
title: 24 Days of Hackage: gloss
---

Today marks the
[20th anniversary of Doom](http://www.wired.com/gamelife/2013/12/john-carmack-doom/),
a game I feel has played a major role in my choice to become a programmer. I
remember the days well of firing up
[DeHackEd](http://doom.wikia.com/wiki/DeHackEd) and
[WAD Author](http://doom.wikia.com/wiki/WadAuthor) and making small checks, and
the joy seeing my changes right there in front of me, ready to interact with in
realtime. Later, I went on to learn how to do game programming, and as they
say - the rest is history. It seems fitting today that we look at another
library that can be useful for interactive multimedia programming, and that
library is [`gloss`](http://hackage.haskell.org/package/gloss).

Created by Ben Lippmeier (who is also known for his work on `repa`), `gloss` is
a high-level library for drawing vector graphics and dealing with interactions,
with the aim of "getting something cool on the screen in under 10
minutes". `gloss` comes with abstractions for mixing colours, creating displays,
dealing with input, and drawing basic primitives - such as lines, circles,
bitmaps, text, and so on. Today, we'll look at creating a simple
[Tic-tac-toe](http://en.wikipedia.org/wiki/Tic-tac-toe) game with a little bit
of artificial "intelligence".

## Getting Something Cool On The Screen

The first thing we need to do is to get a window created to display our
game. `gloss` has a few options for this, depending on what you're trying to
display. A simulation is essentially a movie that cannot be interacted with
that has a fixed timestep, where as an animation is similar but has a variable
timestep. We want interactivity though, so we'll use the "Game" mode. The main
function here is `playIO`:

`playIO` takes a few initial configuration options - the type of display to
create, the target framerate, the background colour and the initial state of the
world (whatever that may be). Our state of the world can a marker of whose turn
it is, and the current board configuration. The board configuration itself is
just a list of list of plays, where the initial state is a board that has no
plays.

```haskell
data Play = X | O deriving Eq

type Board = [[Maybe Play]]

initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Nothing)
```

Now we're in a position to create a window and start working on our game:

```haskell
main :: IO ()
main = do
  playIO
    (InWindow "Tic-tac-toe" (1, 1) (500, 500))
    azure
    10
    (initialBoard, X)
    drawBoard
    handleInput
    stepGame
```

We create a new window with a title, our initial state of an empty
board with "X" is next to play, and "azure" as the background colour. We have to
write three functions now - `drawBoard`, `handleInput` and
`stepGame`. `drawBoard` is straight forward:

```haskell
drawBoard :: (Board, Play) -> IO Picture
drawBoard (board, _) = return (grid <> plays)
 where
  grid = 
    color black (line [ (-100, -300), (-100,  300) ]) <>
    color black (line [ ( 100, -300), ( 100,  300) ]) <>
    color black (line [ (-300,  100), ( 300,  100) ]) <>
    color black (line [ (-300, -100), ( 300, -100) ])

  plays = mconcat
    [ translate (fromIntegral $ (x - 1) * 200)
                (fromIntegral $ (y - 1) * 200) $
        case play of
          X -> color white (thickCircle 1 50)
          O -> color black (thickCircle 1 50)
    | x <- [0..2]
    , y <- [0..2]
    , Just play <- [ (board !! x) !! y ]
    ]
```

`drawBoard` needs to return a `Picture`, which is the type of primitives that
`gloss` can display. `Picture`s along with composition form a `Monoid`, which
means we can easily draw a complex scene by combining simpler `Picture`s
together. In this case, I begin by simply declaring that I want to draw both the
grid and the current plays. The grid is defined to be the combination of 4 lines
(two vertical, two horizontal), while the plays is a little more involved.

To draw the plays, I loop over every cell in the board by the cell's
coordinates - I'll be using the coordinates to work out the translation to draw
the play. We loop over all the coordinates, and attempt to pattern match that
cell against `Just play`. If this fails, the list comprehension will continue,
but if it does succeed, then we can draw a single play in the game. This list
comprehension produces a list of plays, which I can then `mconcat` together -
turning my `[Picture]` into a single `Picture`.

So far so good! But what about playing the game? The next piece of the puzzle is
getting some input, and we can do this with `handleInput`. `handleInput` takes
an `Event` and the state of the world, and can respond to that event by
producing a new state. We're only interested in a specific event - the release
of the left mouse button to signify the user wants to make a move. We need to be
careful though - the user should only be able to make moves if it's their go!
That's why we have the play marker in the game state. We'll deal the case where
it's our turn first:

```haskell
handleInput :: Event -> (Board, Play) -> IO (Board, Play)
handleInput
  (EventKey (MouseButton LeftButton) Up _ (x, y))
  (board, X) = do
```

We use pattern matching in the `handleInput` parameters to make sure it's "X"s
turn, and also that the event is the release of the left mouse button. The next
thing we need to do is convert the mouse coordinates to grid coordinates, and
then we look up those coordinates against our current state. If someone has
already played there then we don't make any changes - otherwise we return an
updated board with an "X" wherever the user clicked, and switch over to "O"s
turn:

```haskell
let snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/ 100) .
           (+ 50)
    (gridX, gridY) = (snap x, snap y)
    
case (board !! gridX) !! gridY of
  Just _ -> return (board, X)

  Nothing -> do
    let newBoard = (ix gridX . ix gridY .~ (Just X)) board
    return (newBoard, O)
```

We're almost there! The final function to implement is just one to step the game
at our framerate - 10 frames a second, as specified in the call to `playIO`. In
this case, there's not really anything to do, so for now we'll just act as
identity:

```haskell
stepGame :: Float -> (Board, Play) -> IO (Board, Play)
stepGame _ = id
```

## Artificial "Intelligence"

Our game isn't very fun right now - once the user clicks on a space we make a
move and switch over to "O"s move - but there is no player "O"! Let's rectify
that with some fairly brain dead AI.

My plan here is to use Haskell's lightweight threading to fork off an AI thread
that will make a choice of which move to play. Because we have a lightweight
thread, we can easily use `threadDelay` to give the illusion that the computer
is "thinking". Two functions will need to change with the addition of AI -
`handleInput` will need indicate that AI needs to play a move, and `stepGame` is
going to need to check if the AI has made a move.

I'll use a `MVar Board` to keep track of the AI. Initially, this `MVar` is
empty - there is no value inside it. When the player makes a move, we fork an AI
thread with the *new* board configuration, and the AI will place a further new
board in the `MVar`, containing its response. `stepGame` will then be
responsible for interleaving all this threading. First, lets have a look at our
AI function:

```haskell
forkAi :: MVar Board -> Board -> IO ()
forkAi aiMove board = void $ forkIO $ do
  -- Pause while we think what to do
  randomRIO (100000, 1000000) >>= threadDelay

  -- Choose a random move
  let plays = [ (ix x . ix y .~ Just O) board
              | x <- [0..2]
              , y <- [0..2]
              , Nothing <- [ (board !! x) !! y ]
              ]

  case plays of
    [] -> do
      -- There are no more moves!
      putMVar aiMove board

    _ -> do
      -- Respond with the move chosen at random
      newBoard <- (plays !!) <$> randomRIO (0, length plays - 1)
      putMVar aiMove newBoard
```

The first thing we do is use `threadDelay` to slow the AI down. Once the delay
has passed we build a list of *all* possible avenues from the current board
configuration. Then we take a random move and respond with that. (There's also a
little bit of checking to make sure we're not in the scenario where there are no
more moves left).

We hook this into `handleInput` by calling `forkIO` before we return a new
board:

```haskell
  ...
  Nothing -> do
    let newBoard = (ix gridX . ix gridY .~ (Just X)) board
    forkAi aiMove newBoard
    return (newBoard, O)
  ...
```

Finally, `stepGame` needs to check if the AI has played their move, so this
function gets a little more complex now:

```haskell
stepGame :: MVar Board -> Float -> (Board, Play) -> IO (Board, Play)
stepGame aiMove _ (board, O) =
  tryTakeMVar aiMove >>=
    return .
      maybe (board, O)
            (\newBoard -> (newBoard, X))

stepGame _ _ state = return state
```

If it's "O"s go, we use `tryTakeMVar` to optimistically check if the AI has made
a move. If they have, we'll have a `Just Board` to use as our new game
state. Otherwise, we'll get `Nothing` back - which means that the AI hasn't made
a choice yet so we should keep the state the same. Here's what we get in the
end:

<div style="text-align: center; margin: 1em 0">
<iframe width="420" height="315" src="//www.youtube.com/embed/KQX9xA-LkRU" frameborder="0" allowfullscreen></iframe>
</div>

## Conclusion

`gloss` is a really fun game programming library, mainly because it really stays
out of the way. As I quoted in the opening paragraphs, `gloss` wants to help you
get things on the screen as fast as possible, and I think it definitely achieves
that. The code I wrote today is hardly production grade Haskell, but it doesn't
matter, because it was *fun* to write. I think when it comes to doing creative
work in Haskell, fun has to come first, as that's where the drive to keep going
comes from. This doesn't mean `gloss` requires you to write bad code either -
but it lets you defer the choices of being "correct" until a later stage.

If you want to take this code a little further, then feel free to grab my work
from [Github](http://github.com/ocharles/blog)! As you will have noticed from
the above video, the game doesn't have any concept of scoring or the end of a
game, which is certainly where work needs to focus. But you might also want to
have a play by changing `drawBoard` to be closer to the classic game using X and
O.
