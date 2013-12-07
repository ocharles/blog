---
title: 24 Days of Hackage: threepenny-gui
---

While a lot of the work I tend to do in Haskell is building libraries, there
have been plenty of times where I would have loved to provide a GUI. Usually, I
shy away from this task, and settle for a command line interface - working with
GUI libraries is usually a nightmare, both in code and deployment! In July
ealrier this year, [Heinrich Apfelmus](http://apfelmus.nfshost.com/)
[announced the release](http://apfelmus.nfshost.com/blog/2013/07/21-threepenny-gui-0-1.html)
of his new GUI library
[`threepenny-gui`](http://hackage.haskell.org/package/threepenny-gui), which can
solve both of these problems. `threepenny-gui` is a Haskell library to build
GUIs, but with a twist: rather than deal with bindings to GTK or QT,
`threepenny-gui` uses an interface that is already almost universally
acceptable - web pages!

`threepenny-gui` is thus a collection of common UI elements, a way of composing
them into web pages, and the ability to watch these interface elements for
interactions. `threepenny-gui` also uses web sockets to provide a tight feedbcak
loop between the client and the server. Today, we'll explore the
`threepenny-gui` library by building a to-do list.

Before we even begin considering how to write the GUI, we should start by
defining a few data types that will sufficiently model the application. We could
use `persistent` for our data store, as we
[at saw yesterday](/posts/2013-12-06-24-days-of-hackage-persistent-esqueleto.html) -
but instead I'll just work with an in-memory database using Haskell types.

```haskell
type Database = Map UserName ToDoList
type UserName = String
type ToDoList = Set String
```

Our "database" will be a map from usernames to to-do lists, where a username is
just a `String` and a to-do list is a `Set` of `String`s - very straight
forward. Next, we can start up a server for our application using
`threepenny-gui`:

```haskell
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup rootWindow = undefined
```

`startGUI` begins a HTTP server, here we use the default configuration which
listens on port 10000. The `setup` action takes a `Window` - which corresponds
to the browser page the client is viewing - and builds up the UI of the entire
application. Every client connection will call `setup` on connection, which
means by default there is no cross-communication or state. To add state and
cross-communication, we need to explicitly introduce a shared variable. To
achieve this sharing, we'll use a mutable variable within the STM framework:

```haskell
main :: IO ()
main = do
  database <- atomically $ newTVar (Map.empty)
  startGUI defaultConfig (setup database)

setup :: TVar Database -> Window -> UI ()
setup database rootWindow = do
```

Now we're in a position to start building our UI. The first stage of the UI is
to prompt the client for their username in order to determine which to-do list
to display to them:

```haskell
setup :: TVar Database -> Window -> UI ()
setup database rootWindow = void $ do
  userNameInput <- UI.input
    # set (attr "placeholder") "User name"

  loginButton <- UI.button #+ [ string "Login" ]

  getBody rootWindow #+
    map element [ userNameInput, loginButton ]
```

First of all we call `UI.input`, which creates a new `<input>` element. The `#`
operator is reverse function application, which we use to then change the
placeholder attribute of our newly created text field. To create our login
button, we create a button element with `UI.button`, and then we add a string
element as a child of the button element in order to set the text content.

Now that we have both of these UI elements, we can append them to the body of
the `rootWindow` UI element. `getBody` takes a `Window` and returns the
corresponding `Element` for the window, which we can append to using `#+`.

Next, we need to respond to the user clicking the "login" button, which we do by
observing the `click` event:

```haskell
  on UI.click loginButton $ \_ -> do
```

Now, what do we need to do when someone clicks "login"? We need to look up that
username in our database and, if they exist, present a list of to-do items the
user has created. We should also show an input field to add new to-do items. The
first part is straight forward:

```haskell
    userName <- get value userNameInput

    currentItems <- fmap Set.toList $ liftIO $ atomically $ do
      db <- readTVar database
      case Map.lookup userName db of
        Nothing -> do
           writeTVar database (Map.insert userName Set.empty db)
           return Set.empty

        Just items -> return items
```

We get the value of the `userNameInput` input field, and then enter an STM
transaction to find all the to-do items. We attempt to lookup the user in the
database, and if they have items we return them. Otherwise, we "register" a new
user and give them an empty set of to-do items. Next, we render the user's to-do
items:

```haskell
    let showItem item = UI.li #+ [ string item ]
    toDoContainer <- UI.ul #+ map showItem currentItems
```

Again, straight forward! I just map a `showItem` routine over each to-do entry,
and contain them all in a `<ul>` container. We'll need to add new entries to
this list later, so we'll bind the `<ul>` element to a name.

The last bit of functionality is the ability to create new to-do items. For
this, lets use another `<input>` element, and when the user presses "return"
have the item be added to the list:

```haskell
    newItem <- UI.input

    on UI.sendValue newItem $ \input -> do
      liftIO $ atomically $ modifyTVar database $
        Map.adjust (Set.insert input) userName

      set UI.value "" (element newItem)
      element toDoContainer #+ [ showItem input ]
```

A new input element is created, and we listen for the `sendValue` event, which
is produced when a client presses return. Then we run a little STM transaction
to add a new item to the to-do list. Once this transaction completes, we clear
the input field and append the newly created to-do item to the to-do list.

Finally, we combine this all together into a UI:

```haskell
    header <- UI.h1 #+ [ string $ userName ++ "'s To-Do List" ]
    set children
      [ header, toDoContainer, newItem ]
      (getBody rootWindow)
```

And we're done! If we head over to `http://localhost:10000` we're presented with
a username field, and if we fill this in we'll be shown a list of to-do
items. Changing the values and then logging in as the same user again shows the
items have persisted, just as we were intending.

Today is my first exposure to `threepenny-gui`, and overall I really like what
I'm seeing! In a little over 50 lines of code we have a functioning application,
and I feel I understand a lot of what I'm doing. There are a few parts that
still feel a little odd - for example passing around `UI Element`s, where (to
me) it would feel more natural to just pass an `Element` instead. Nonetheless,
that's a minor concern that shouldn't stop you experimenting with this fantastic
project.

I also started my first draft of this post raving on about FRP, but observent
readers will notice we didn't really use any of the FRP functionality that
`threepenny-gui` offers. Sadly, I didn't get time to touch this part of the
library, and I think it has the potential to do more interesting things. For
example, making the to-do list collaborative by reacting to asynchronous changes
to the to-do list.

As always, the code for today's example is
[over on Github](https://github.com/ocharles/blog/blob/master/code/2013-12-07-threepenny-gui.hs) -
feel free to have a play and see what you come up with!
