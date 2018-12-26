---
title: Solving Planning Problems with Fast Downward and Haskell
---

In this post I'll demonstrate my new
[`fast-downward`](https://hackage.haskell.org/package/fast-downward) library and
show how it can be used to solve planning problems. The name comes from the use
of the backend solver - [Fast Downward](http://fast-downward.org). But what's a
planning problem?

Roughly speaking, planning problems are a subclass of AI problems where we need
to work out a *plan* that moves us from an initial state to some goal state.
Typically, we have:

* A known *starting state* - information about the world we know to be true right
  now.
* A set of possible *effects* - deterministic ways we can change the world.
* A *goal* state that we wish to reach.

With this, we need to find a plan:

* A *solution* to a planning problem is a *plan* - a totally ordered sequence of
  steps that converge the starting state into the goal state.

Planning problems are essentially [state space
search](https://en.wikipedia.org/wiki/State_space_search) problems, and crop up
in all sorts of places. The common examples are that of moving a robot around,
planning logistics problems, and so on, but they can be used for plenty more!
For example, the [Beam](http://tathougies.github.io/beam/) library uses state
space search to work out how to converge a database from one state to another
(automatic migrations) by adding/removing columns.

State space search is an intuitive approach - simply build a graph where nodes
are states and edges are state transitions (effects), and a find a path
(possibly shortest) that gets you from the starting state to a state that
satisfies some predicates. However, naive enumeration of all states rapidly
grinds to a halt. Forming optimal plans (least cost, least steps, etc) is an
extremely difficult problem, and there is a *lot* of literature on the topic
(see [ICAPS](http://icaps-conference.org) - the International Conference on
Automated Planning and Scheduling and recent [International Planning
Competitions](https://ipc2018.bitbucket.io/) for an idea of the state of the
art). The `fast-downward` library uses the state of the art Fast Downward solver
and provides a small DSL to interface to it with Haskell.

In this post, we'll look at using `fast-downward` in the context of solving a
small planning problem - moving balls between rooms via a robot. This post is
literate Haskell, here's the context we'll be working in:

```haskell
{-# language DisambiguateRecordFields #-}

module FastDownward.Examples.Gripper where

import Control.Monad
import qualified FastDownward.Exec as Exec
import FastDownward.Problem
```

If you'd rather see the Haskell in it's entirety without comments, simply head
to the end of this post.

## Modelling The Problem

### Defining the Domain

As mentioned, in this example, we'll consider the problem of transporting balls
between rooms via a robot. The robot has two grippers and can move between
rooms. Each gripper can hold zero or one balls. Our initial state is that
everything is in room A, and our goal is to move all balls to room B.

First, we'll introduce some domain specific types and functions to help model
the problem. The `fast-downward` DSL can work with any type that is an instance
of `Ord`.

```haskell
data Room = RoomA | RoomB
  deriving (Eq, Ord, Show)

adjacent :: Room -> Room
adjacent RoomA = RoomB
adjacent RoomB = RoomA

data BallLocation = InRoom Room | InGripper
  deriving (Eq, Ord, Show)

data GripperState = Empty | HoldingBall
  deriving (Eq, Ord, Show)
```

A ball in our model is modelled by its current location. As this changes over
time, it is a `Var` - a state variable.

```haskell
type Ball = Var BallLocation
```

A gripper in our model is modelled by its state - whether or not it's holding a
ball.

```haskell
type Gripper = Var GripperState
```

Finally, we'll introduce a type of all possible actions that can be taken:

```haskell
data Action = PickUpBall | SwitchRooms | DropBall
  deriving (Show)
```

With this, we can now begin modelling the specific *instance* of the problem. We
do this by working in the `Problem` monad, which lets us introduce variables
(`Var`s) and specify their initial state.

### Setting the Initial State

```haskell
problem :: Problem (SolveResult Action)
problem = do
```

First, we introduce a state variable for each of the 4 balls. As in the problem
description, all balls are initially in room A.

```haskell
  balls <- replicateM 4 (newVar (InRoom RoomA))
```

Next, introduce a variable for the room the robot is in - which also begins in
room A.

```haskell
  robotLocation <- newVar RoomA
```

We also introduce variables to track the state of each gripper.

```haskell
  grippers <- replicateM 2 (newVar Empty)
```

This is sufficient to model our problem. Next, we'll define some effects to
change the state of the world.

### Defining Effects

Effects are computations in the `Effect` monad - a monad that allows us to read
and write to variables, and also fail (via `MonadPlus`). We could define these
effects as top-level definitions (which might be better if we were writing a
library), but here I'll just define them inline so they can easily access the
above state variables.

Effects may be used at any time by the solver. Indeed, that's what solving
planning problems is all about! The hard part is choosing effects intelligently,
rather than blindly trying everything. Fortunately, you don't need to worry
about that - Fast Downward will take care of that for you!

```haskell
  let
```

#### Picking Up Balls

The first effect takes a ball and a gripper, and attempts to pick up that ball
with that gripper.

```haskell
    pickUpBallWithGrippper :: Ball -> Gripper -> Effect Action
    pickUpBallWithGrippper b gripper = do
      Empty <- readVar gripper                  -- (1)

      robotRoom <- readVar robotLocation        -- (2)
      ballLocation <- readVar b
      guard (ballLocation == InRoom robotRoom)  -- (3)

      writeVar b InGripper                      -- (4)
      writeVar gripper HoldingBall

      return PickUpBall                         -- (5)
```

1. First we check that the gripper is empty. This can be done concisely by using
an incomplete pattern match. `do` notation desugars incomplete pattern matches
to a call to `fail`, which in the `Effect` monad simply means "this effect can't
currently be used".

2. Next, we check where the ball and robot are, and make sure they are both in
the same room.

3. Here we couldn't choose a particular pattern match to use, because picking up
a ball should be possible in either room. Instead, we simply observe the
location of both the ball and the robot, and use an equality test with `gurad`
to make sure they match.

4. If we got this far then we can pick up the ball. The act of picking up the
ball is to say that the ball is now in a gripper, and that the gripper is now
holding a ball.

5. Finally, we return some domain specific information to use if the solver
chooses this effect. This has no impact on the final plan, but it's information
we can use to execute the plan in the real world (e.g., sending actual commands
to the robot).

#### Moving Between Rooms

This effect moves the robot to the room adjacent to its current location.

```haskell
    moveRobotToAdjacentRoom :: Effect Action
    moveRobotToAdjacentRoom = do
      modifyVar robotLocation adjacent

      return SwitchRooms
```

This is an "unconditional" effect as we don't have any explicit guards or
pattern matches. We simply flip the current location by an adjacency function.

Again, we finish by returning some information to use when this effect is
chosen.

#### Dropping Balls

Finally, we have an effect to drop a ball from a gripper.

```haskell
    dropBall :: Ball -> Gripper -> Effect Action
    dropBall b gripper = do
      HoldingBall <- readVar gripper     -- (1)
      InGripper <- readVar b

      robotRoom <- readVar robotLocation -- (2)
      writeVar gripper Empty             -- (3)
      writeVar b (InRoom robotRoom)      -- (4)

      return DropBall                    -- (5)
```

1. First we check that the given gripper is holding a ball, and the given ball is
in a gripper.

2. If we got here then those assumptions hold. We'll update the location of the
ball to be the location of the robot, so first read out the robot's location.

3. Empty the gripper

4. Move the ball.

5. And we're done! We'll just return a tag to indicate that this effect was
   chosen.

## Solving Problems

With our problem modelled, we can now attempt to solve it. We invoke `solve`
with a particular search engine (in this case A* with landmark counting
heuristics). We give the solver two bits of information:

1. A list of all effects - all possible actions the solver can use. These are
   precisely the effects we defined above, but instantiated for all balls and
   grippers.
2. A goal state. Here we're using a list comprehension which enumerates all
   balls, adding the condition that the ball location must be `InRoom RoomB`.

```haskell
  solve
    cfg
    ( [ pickUpBallWithGrippper b g | b <- balls, g <- grippers ]
        ++ [ dropBall b g | b <- balls, g <- grippers ]
        ++ [ moveRobotToAdjacentRoom ]
    )
    [ b ?= InRoom RoomB | b <- balls ]
```

So far we've been working in the `Problem` monad. We can escape this monad by
using `runProblem :: Problem a -> IO a`. In our case, `a` is `SolveResult Action`,
so running the problem might give us a plan (courtesy of `solve`). If it did,
we'll print the plan.

```haskell
main :: IO ()
main = do
  res <- runProblem problem
  case res of
    Solved plan -> do
      putStrLn "Found a plan!"
      zipWithM_ 
        ( \i step -> putStrLn ( show i ++ ": " ++ show step ) ) 
        [ 1::Int .. ] 
        ( totallyOrderedPlan plan )

    _ ->
      putStrLn "Couldn't find a plan!"
```

`fast-downward` allows you to extract a totally ordered plan from a solution,
but can also provide a `partiallyOrderedPlan`. This type of plan is a graph
(partial order) rather than a list (total order), and attempts to recover some
concurrency. For example, if two effects do not interact with each other, they
will be scheduled in parallel.


## Well, Did it Work?!

All that's left is to run the problem!

```
> main
Found a plan!
1: PickUpBall
2: PickUpBall
3: SwitchRooms
4: DropBall
5: DropBall
6: SwitchRooms
7: PickUpBall
8: PickUpBall
9: SwitchRooms
10: DropBall
11: DropBall
```

Woohoo! Not bad for 0.02 secs, too :)

## Behind The Scenes

It might be interesting to some readers to understand what's going on behind the
scenes. Fast Downward is a C++ program, yet somehow it seems to be running
Haskell code with nothing but an `Ord` instance - there are no marshalling types
involved!

First, let's understand the input to Fast Downward. Fast Downward requires an
encoding in its own SAS format. This format has a list of variables, where each
variable contains a list of values. The contents of the values aren't actually
used by the solver, rather it just works with indices into the list of values
for a variable. This observations means we can just invent values on the Haskell
side and careful manage mapping indices back and forward.

Next, Fast Downward needs a list of operators which are ground instantiations of
our effects above. Ground instantiations of operators mention exact values of
variables. Recounting our gripper example, `pickUpBallWithGrippper b gripper`
actually produces 2 operators - one for each room. However, we didn't have to be
this specific in the Haskell code, so how are we going to recover this
information?

`fast-downward` actually performs expansion on the given effects to find out
*all* possible ways they could be called, by non-deterministically evaluating
them to find a fixed point.

A small example can be seen in the `moveRobotToAdjacentRoom` `Effect`. This will
actually produce two operators - one to move from room A to room B, and one to
move from room A to room B. The body of this `Effect` is (once we inline the
definition of `modifyVar`)

```haskell
  readVar robotLocation >>= writeVar robotLocation . adjacent
```

Initially, we only know that `robotLocation` can take the value `RoomA`, as that
is what the variable was initialised with. So we pass this in, and see what the
rest of the computation produces. This means we evaluate `adjacent RoomA` to
yield `RoomB`, and write `RoomB` into `robotLocation`. We're done for the first
pass through this effect, but we gained new information - namely that
`robotLocation` *might* at some point contain `RoomB`. Knowing this, we then
rerun the effect, but the first `readVar` gives us two paths:

```haskell
readVar robotLocation 
  >>= \RoomA -> writeVar robotLocation RoomB                     -- If we read RoomA
  >>= \RoomB -> writeVar robotLocation (adjacent RoomB -> RoomA) -- If we read RoomB
```

This shows us that `robotLocation` might also be set to `RoomA`. However, we
already knew this, so at this point we've reached a fixed point.

In practice, this process is ran over all `Effect`s at the same time because
they may interact - a change in one `Effect` might cause new paths to be found
in another `Effect`. However, because `fast-downward` only works with finite
domain representations, this algorithm always terminates. Unfortunately, I have
no way of enforcing this that I can see, which means a user *could* infinitely
loop this normalisation process by writing `modifyVar v succ`, which would
produce an infinite number of variable assignments.

## Conclusion

CircuitHub are using this in production (and I mean real, physical production!)
to coordinate activities in its factories. By using AI, we have a declarative
interface to the production process - rather than saying what steps are to be
performed, we can instead say what state we want to end up in and we can trust
the planner to find a suitable way to make it so.

Haskell really shines here, giving a very powerful way to present problems to
the solver. The industry standard is PDDL, a Lisp-like language that I've found
in practice is less than ideal to actually encode problems. By using Haskell,
we:

* Can easily feed the results of the planner into a scheduler to execute the
  plan, with no messy marshalling. 
* Use well known means of abstraction to organise the problem. For example, in
  the above we use Haskell as a type of macro language - using do notation to
  help us succinctly formulate the problem.
* Abstract out the details of planning problems so the rest of the team can
  focus on the domain specific details - i.e., what options are available to the
  solver, and the domain specific constraints they are subject to.

[`fast-downward`](https://hackage.haskell.org/package/fast-downward) is
available on Hackage now, and I'd like to express a huge thank you to CircuitHub
for giving me the time to explore this large space and to refine my work into
the best solution I could think of. This work is the result of numerous
iterations, but I think it was worth the wait!

## Appendix: Code Without Comments

Here is the complete example, as a single Haskell block:

```haskell
{-# language DisambiguateRecordFields #-}

module FastDownward.Examples.Gripper where

import Control.Monad
import qualified FastDownward.Exec as Exec
import FastDownward.Problem


data Room = RoomA | RoomB
  deriving (Eq, Ord, Show)


adjacent :: Room -> Room
adjacent RoomA = RoomB
adjacent RoomB = RoomA


data BallLocation = InRoom Room | InGripper
  deriving (Eq, Ord, Show)


data GripperState = Empty | HoldingBall
  deriving (Eq, Ord, Show)


type Ball = Var BallLocation


type Gripper = Var GripperState

  
data Action = PickUpBall | SwitchRooms | DropBall
  deriving (Show)


problem :: Problem (Maybe [Action])
problem = do
  balls <- replicateM 4 (newVar (InRoom RoomA))
  robotLocation <- newVar RoomA
  grippers <- replicateM 2 (newVar Empty)

  let
    pickUpBallWithGrippper :: Ball -> Gripper -> Effect Action
    pickUpBallWithGrippper b gripper = do
      Empty <- readVar gripper
  
      robotRoom <- readVar robotLocation
      ballLocation <- readVar b
      guard (ballLocation == InRoom robotRoom)
  
      writeVar b InGripper
      writeVar gripper HoldingBall
  
      return PickUpBall


    moveRobotToAdjacentRoom :: Effect Action
    moveRobotToAdjacentRoom = do
      modifyVar robotLocation adjacent
      return SwitchRooms


    dropBall :: Ball -> Gripper -> Effect Action
    dropBall b gripper = do
      HoldingBall <- readVar gripper
      InGripper <- readVar b
  
      robotRoom <- readVar robotLocation
      writeVar b (InRoom robotRoom)
  
      writeVar gripper Empty
  
      return DropBall

  
  solve
    cfg
    ( [ pickUpBallWithGrippper b g | b <- balls, g <- grippers ]
        ++ [ dropBall b g | b <- balls, g <- grippers ]
        ++ [ moveRobotToAdjacentRoom ]
    )
    [ b ?= InRoom RoomB | b <- balls ]

  
main :: IO ()
main = do
  plan <- runProblem problem
  case plan of
    Nothing ->
      putStrLn "Couldn't find a plan!"

    Just steps -> do
      putStrLn "Found a plan!"
      zipWithM_ (\i step -> putStrLn $ show i ++ ": " ++ show step) [1::Int ..] steps


cfg :: Exec.SearchEngine
cfg =
  Exec.AStar Exec.AStarConfiguration
    { evaluator =
        Exec.LMCount Exec.LMCountConfiguration
          { lmFactory =
              Exec.LMExhaust Exec.LMExhaustConfiguration
                { reasonableOrders = False
                , onlyCausalLandmarks = False
                , disjunctiveLandmarks = True
                , conjunctiveLandmarks = True
                , noOrders = False
                }
          , admissible = False
          , optimal = False
          , pref = True
          , alm = True
          , lpSolver = Exec.CPLEX
          , transform = Exec.NoTransform
          , cacheEstimates = True
          }
    , lazyEvaluator = Nothing
    , pruning = Exec.Null
    , costType = Exec.Normal
    , bound = Nothing
    , maxTime = Nothing
    }
```
