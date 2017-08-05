---
title: "Quick and Easy DSLs with Writer Endo"
---

In the last week, I’ve had to briefly take off my beloved programmer hat and
wear my slightly less fetching management hat. We have a complex project in the
works and need to do some planning to understand which tasks are critical, which
are flexible, and what options we have to realise the plan. I ended up using
[TaskJuggler](http://taskjuggler.org/) but couldn’t help but wonder - how would
this look if Haskell was my declaration language?

If you’ve never used TaskJuggler, its a great piece of software that uses its
own language to declare the topology of a project, if you will. You are given
primitives such as tasks and projects, in order to create a dependency graph
that can then be scheduled. For example, a minimal project might be:

```
project worldDom "World domination"
task write "Write blog post"
task post "Post to reddit"
task unknown "???"
task profit "Acquire profit"
```

In this post, we’ll look at mimicking this type of DSL in Haskell.

Before we can begin writing our language, we need to have the data structures
that the language will use as building blocks. Here is a plausible model:

```haskell
data Project = Project { projectName :: String
                       , projectStartDate :: Day
                       , projectEndDate :: Maybe Day
                       , projectTasks :: [Task]
                       }

data Task = Task { taskName :: String }
```

Armed with these data types, we are ready to build our DSL. Translating the
original example, I desire to write something like the following:

```haskell
worldDom <- project "World domination" startDate $ do
  task "Write blog post"
  task "Post to reddit"
  task "???"
  task "Acquire profit"
```

The syntax is clean, and hopefully readable by non-Haskell programmers. How can
we realise this syntax?

A good tool for this is the `Writer` monad, a monad which allows us to
accumulate changes in some monoid. But which monoid? Is `Project` a monoid?
Sadly, no. Notice how we made the start date of the project required - in order
to be able to make our `Project` into a monoid we would need some concept of the
empty `Project`, and we’re stuck needing the ill-defined “empty date”.

So if `Project` isn’t a monoid, is there anyway to use it in a monoid? There
certainly is, and before we look at the answer, let’s consider the following:

```haskell
withTask :: Task -> Project -> Project
withTask t p = p { projectTasks = projectTasks p <> [t] }

worldDom =
  withTask "Write blog post"
    (withTask "Post to reddit"
      (withTask "???"
        (withTask "Acquire profit"
          Project { ... })))
```

Notice how we’re chaining together a bunch of transformations on a `Project`? This
looks like a monoid! But wait, you suspiciously call out, what about the empty
element? That would have to take a `Project` and perform no modifications - the
identity transformation. We have that element at our disposal too, its just the
standard `id` function from the prelude.

The monoid of repeated `a -> a` transformations along with `id` as the empty
element is called the `Endo a` monoid, and we’re specifically interested in
`Endo Project`.

Finally, we have our monoid, and we’re ready to define our first construct,
`task`. `task` should take a project to itself, but with one more task
appended. This transformation is what we defined as `withTask` earlier. All we
need to do now, is this wrap this transformation up into `Endo` and write into
our monad:

```haskell
task :: String -> Writer (Endo Project) Task
task name = do
    tell $ Endo (\p -> withTask t p)
    return t
  where
    t = Task { taskName = name }
    withTask t p = p { projectTasks = projectTasks p <> [t] }
```

I’ve also made the task construct return the newly constructed task, which we
allow us to refer to this task in other tasks. For example, we might later
extend our DSL to support something like:

```haskell
a <- task "A" (pure ())
b <- task "B" $ do
  dependOn a
```

The last step is to run all of this to produce a final `Project` value, and we
do this through the `project` function:

```haskell
project :: String -> Day -> Writer (Endo Project) a -> Project
project name start projectBuilder = appEndo (execWriter projectBuilder)
  Project { projectName = name
          , projectStartDate = start
          , projectEndDate = Nothing
          , projectTasks = mempty
          }
```

We take our project builder - the `Writer (Endo Project) a` action, and run it for
its side effects discarding the result. This yields a function that is
`Project -> Project`, so we simply apply a start `Project` to this, and are
returned a `Project` with all the transformations we requested.

Now that we’ve seen a general technique for building DSLs on top of pretty much
any data type, its worth asking what we gain from doing so. Amongst other
things, some advantages are:

* Monad transformers compose.

  For example, we could also use `WriterT (Endo Project) IO` as our monad, which
  would let us perform arbitrary IO while defining the tasks of a project. We
  might take advantage of this to query a bug tracker, and create a project from
  known bug reports.

* Shallow embedding gives us many tools for free.

  While the final project tree we build up is a
  [deep embedding](http://www.haskell.org/haskellwiki/Embedded_domain_specific_language#Degree_of_embedding),
  we have a somewhat shallow embedding in the monad. This lets use take
  advantage of all the existing Haskell functions to build up our project. For
  example, if we want to conditionally include a task, we can use
  `Control.Monad.when`, and if we want to include a list of tasks, we can use
  `Control.Monad.mapM`.

* We don’t need to write a parser.

  The less code we have to write, the less bugs we risk introducing! By using a
  `Writer` monad, we essentially got the parser ‘for free’ - it’s the Haskell
  parser itself.

All of this is very new to me, so if you have other thoughts on how this DSL
could be nicer, I’d love to hear your thoughts.
