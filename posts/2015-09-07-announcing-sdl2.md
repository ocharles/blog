---
title: Announcing a new set of high-level SDL2 bindings
---

It's with great pleasure that on behalf of the haskell-game group, I'd like to announce the release of a new set of high-level bindings to the [SDL](http://libsdl.org) library. SDL is a C library providing a set of cross-platform functions for handling graphics, window management, audio, joystick/gamepad interaction, and more.

For a while, we've had bindings to SDL 2 on Hackage, but these bindings are as close to 1:1 as you can get in Haskell. This results in a library that certainly *can* be used in Haskell, but does not feel particularly like writing ordinary Haskell! A real concern here is that this raises the barrier to entry for those new to either game programming or writing games in Haskell (or both!) - a barrier that I would certainly like to see lowered. To address this, myself and many others have spent the last year working on high-level bindings to abstract away the C-like feel of the existing library, and to present a more Haskell interface.

To give you an idea of how things look, here's a basic application that opens a window, clears the screen, and quits when the user presses 'q':

```haskell
import SDL

main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == KeyDown &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = not (null (filter eventIsQPress events))
  renderDrawColor renderer $= V4 0 0 1 1
  renderClear renderer
  renderPresent renderer
  unless qPressed (appLoop renderer)
```

Hopefully you'll agree that the code above is close to idiomatic Haskell.

We've tried to be extensive with the bindings, and at the moment the following should (!) all be working:

* Graphics routines have been our highest priority. The bindings should give you full control over window management, access to SDL's new hardware-accelerated 2D rendering routines, and also the ability to set up an OpenGL context (which is compatible with the `OpenGL` and `gl` libraries).
* SDL's audio abstraction is available, and the bindings allow you to open audio devices and stream audio data.
* A clean implementation of SDL's event system, designed for use with pattern matching.
* Access to input devices, including keyboard, mouse, pointer devices, and joysticks.
* A large collection of example code, ported from the popular "lazyfoo" and "twinklebear" tutorials.

The bindings are not 100% exhaustive - we've omitted some routines that are already provided by the Haskell runtime, but we also currently lack bindings to the following:

* Force-feedback (SDL's "haptic" functionality). While we do have some code in the repository here, none of the contributors own a device that is compatible with SDL2 to actually test this work. If you do, please drop us a line and help out!
* Gesture recording for touch screens. We're currently targetting desktops and laptops, but SDL has support for Android and iOS. Hopefully when GHC is easier to target these devices, we can start to explore these SDL bindings.
* Other SDL2 compatible libraries, such as `SDL2_net` and `SDL2_ttf`. We'd love for these projects to have the same treatment, and are more than happy to host them under the `haskell-game` Github account.

We hope this enables more people to begin building interactive software and games in Haskell. It's still early days for these bindings, so if you find any bugs (runtime problems or API bugs), or if you find the bindings lacking in anyway, please don't hesitate to [open an issue](https://github.com/haskell-game/sdl2) on our issue tracker.

Happy hacking!
