---
title: How I Develop with Nix
---

[Recently](http://www.reddit.com/r/haskell/comments/1vghgw/nixos_a_gnulinux_distribution_based_on_purely)
NixOS was posted to the Haskell reddit, and seems to have been received well. In
the comments I mentioned that NixOS allows me to be a more productive Haskell
developer, and I would like to discuss how I am using the features offered by
NixOS. Before we get into those details, allow me to briefly summarise the NixOS
project.

Discussing NixOS can be a little confusing due to a few different projects
sharing similar names. I will use the following terminology:

* *Nix*: the package manager that installs software, and the associated
  programming language. This project can be installed on any Linux distribution,
  along with OS X.

* *Nixpkgs*: the common repository of software that defines how to install GHC,
  Emacs, etc.

* *NixOS*: an operating system that uses all the power of Nix and Nixpkgs, but
  also provides configuration management.

In this article, we'll only be focusing on Nix and Nixpkgs, so you should be
able to use these techniques even if you don't want to run NixOS (but you
totally should run NixOS).

[NixOS](http://nixos.org) is a Linux distribution with an emphasis on
*purity*. Rather than having a package manager that works by mutating some
global state (all installed packages), NixOS works by installing software into a
"store", and then viewing this store via symlinks to create profiles. Each entry
in the store is the result of calling a function that describes how to build the
software: the input to the function is source code, build tools and other
dependencies; and the output is the resulting binaries. Therefore, in a very
mathematical sense of the word *function*, Nix installations are reproducible:
if you provide exactly the same inputs to the function, the resulting binary
will always be the same. (I don't want to focus on the details of exactly how
this all works, as I think this implementation information can detract from the
general idea. Interested readers are encouraged to read the paper
[NixOS: A Purely Functional Linux Distribution](http://nixos.org/~eelco/pubs/nixos-jfp-final.pdf)).

In NixOS, we write these functions using the Nix language. This language has a
minimal syntax, with support for function abstraction, attribute sets (which are
like maps/dictionaries in other languages), and a few literals such as strings
and integers. The canonical example is how we package
[GNU hello](https://www.gnu.org/software/hello/):

```
{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "hello-2.9";

  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "19qy37gkasc4csb1d3bdiz9snn8mir2p3aj0jgzmfv0r2hi7mfzc";
  };
}

```

GNU hello is packaged as a function of two parameters: `stdenv`, which provides
a standard library with support for performing the normal Linux `./configure`,
`make`, `make install` dance; and `fetchurl`, a Nix helper function to download
source code and verify it against a SHA (which is required to ensure that the
downloaded source is always the same).

The function body calls the `stdenv.mkDerivation` function to do the heavy
lifting. In this case, we call `stdenv.mkDerivation` with two arguments - `name`
and `src`. The `src` argument is the result of calling `fetchurl`, and
`fetchurl` in turn is a function requiring a `url` and `sha256`. It returns a
file system path to the downloaded source code.

Finally, in Nix we compose all of these individual functions into a large
package repository. This repository essentially calls every single top level
function, with support for recursive bindings in order to satisfy
dependencies. Continuing with the hello example, we may have a top-level entry
point like:

```
rec {
  hello = import /path/to/hello.nix { inherit stdenv fetchurl; };

  stdenv = import /path/to/stdenv.nix { inherit gcc };

  fetchurl = import /path/to ;

  gcc = import /path/to/gcc.nix {};

  # and so on
}
```

(`import` loads a file containing a function and then calls that function with
the provided arguments)

But wait - I just said this calls *all* functions... so wouldn't that then mean
that *all* software gets installed? The trick here is that Nix is a lazy
language. If you never request any of these top-level attributes, then you'll
never do any work! It's only when we install software that we demand the value
of one of these functions, and therefore only the minimal amount of work is
done.

So far so good, but the majority of the readers of this blog aren't interested
in packaging GNU hello - we want to develop Haskell packages! Now that we've
seen what the Nix language looks like, we can move on to see how Nix is useful
for working with Haskell.

## Nix & Haskell

It probably won't surprise readers that NixOS has a lot of Haskell support - the
similarity between lazy evaluation and purity is at least of interest to Haskell
programmers, not to mention our constant interest in unorthodox
solutions. Owing to this interest, Nix comes with some great support for
Haskell, with a large portion of Hackage already packaged, and we have a
`cabal.mkDerivation` which abstracts the process of installing software with
Cabal.

As a quick example of the function for a Haskell package, here is the definition
for `attoparsec` (omitting the `meta` information):

```
{ cabal, deepseq, QuickCheck, scientific, testFramework
, testFrameworkQuickcheck2, text
}:

cabal.mkDerivation (self: {
  pname = "attoparsec";
  version = "0.11.1.0";
  sha256 = "09mks6lbzmqmdz6s10lvdklmc0mydd1sk5hphhnybp3yr4pvh7jc";
  buildDepends = [ deepseq scientific text ];
  testDepends = [
    QuickCheck testFramework testFrameworkQuickcheck2 text
  ];
})

```

`attoparsec` is a function that requires several Haskell libraries in order to
be built, so we make these libraries parameters to the function. Then we just
use `cabal.mkDerivation`, which will download the source code from Hackage (or a
mirror) and use `cabal-install` to build the source code and documentation.

## nix-shell

When we are developing software, it's painful to have to go through the process
of installing before you can do any testing. Not only that, often you want to
test by running GHCI. Can Nix help here?

Nix comes with a binary called `nix-shell` which drops users into a `bash` shell
configured with the same environment that would be used to perform the build
itself. For example, we can drop into a shell that is configured to build
`attoparsec`. Below I'm calling `nix-shell` from my home directory where I
*don't* have GHC installed, but look what happens...


```
> ghci
ghci: command not found

> nix-shell -A haskellPackages.attoparsec

> ghci

ghci> import Data.Text
ghci> :t pack "Hello!"
Text
```

Magic! Not only have we managed to sandbox the installation of `text`, we've
gone even further and sandboxed the whole of GHC! Very cool.

## Project Sandboxes

This brings us to the bit you're all waiting for: per-project support. I use a
variant of the following `default.nix` file in each of my Haskell projects:

```
{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall_1_18_0_2
    text mtl transformers; # Haskell dependencies here

in cabal.mkDerivation (self: {
  pname = "project-name";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    # As imported above
    text mtl transformers
  ];
  buildTools = [ cabalInstall_1_18_0_2 ];
  enableSplitObjs = false;
})
```

This is a little different from the `attoparsec` packaging above, so allow me to
explain what's going on. My project definitions are functions that depend on
`haskellPackages` as defined in Nix itself - this variable contains our
"official" Haskell package repository. I want to re-use as much as I can, so
there's no point re-packaging all the build dependencies. By default,
`haskellPackages` is `haskellPackages` from the Nix repository, though it can be
overridden as we'll see later.

Next, I bring some libraries into scope. `inherit` is essentially syntax sugar;
the above has the same result as:

```
let
  cabal = haskellPackages.cabal;
  text = haskellPackages.text;
  # And so on
```

Finally, I use `cabal.mkDerivation` to build my project. However, rather than
supplying a `sha256`, I manually provide the `src` parameter and point it the
current directory. Now I can easily drop into a shell to work on my project:

```
> cabal --version
cabal: command not found

> nix-shell

> cabal --version
cabal-install version 1.18.0.2
```

### Cross Project Dependencies

Nix really shines for me when it comes to working with projects that have a
bunch of dependencies that aren't on Hackage. In this scenario, I add my build
time dependencies to the parameters of `default.nix`. Let's imagine I have some
common definitions in `ocharles-common` - I just add another parameter:

```
{ haskellPackages ? (import <nixpkgs> {}).haskellPackages
, ocharlesCommon
}:
```

However, I can't yet use `nix-shell`:

```
> nix-shell
error: cannot auto-call a function that has an argument without a default value (`ocharlesCommon`)
```

This is because `nix-shell` can only execute functions with no parameters (or
defaults for each parameter). To solve this, we just need to provide an
argument:

```
> nix-shell --arg ocharlesCommon 'import ../ocharles-common {}'
```

This will build `ocharles-common` and then drop us into a shell ready to carry
on development. I tend to automate this by having `my-env.nix` which calls
`default.nix` with default arguments.

### Profiling

Profiling in Haskell is always a bit of a pain, because you have to ensure all
your dependencies have been built with profiling. However, because we've
abstracted `haskellPackages` out, it's easy to switch to profiling mode in
NixOS. Now we just need to run `nix-shell` overriding the `haskellPackages`
attribute:

```
> nix-shell --arg haskellPackages 'with import <nixpkgs> {}; haskellPackages_ghc763_profiling'
```

Here we'll be dropped into a shell with GHC 7.6.3 and all dependencies built
with profiling.

### Emacs Integration

While not particularly interesting, I use all of this with Emacs. I use
[projectile](https://github.com/bbatsov/projectile), and when I want to build my
source code I just hit `C-c p c` to run `compile` from the top-level directory
of my project. I specify the following compile command:

```
nix-shell --pure --command 'cabal build'
```

Cabal then builds my project, and I can use Emacs to jump to any source code
errors.  I only have to type this in once per Emacs session - `projectile`
remembers it for future invocations. I think with a `.projectile` file I can
probably automate even that away, but I haven't had much need to investigate
that yet.


### What Nix Buys Me

You might be thinking "this seems pretty cool, but is it much better than
`cabal sandbox`?" In my opinion, yes, it is. Here's why:

* *The pure model inside Nix allows for binary substitution*. This means
  that rather than building a whole library from source, you can simply download
  pre-built binaries instead. We do this by generating a "hash" of a package,
  which includes all dependencies and source code, and then just look this hash
  up in our build farm.

* *The binary substitution means that you can cache local builds*. If the
  hash is unchanged and the store already contains that hash, then there's no
  point rebuilding it. Thus Nix can drastically speed up development time by
  doing practically no work. It's only when you actually make a change will a
  library be re-installed. This approach also means you don't even have to think
  about rebuilding things, because that will happen when it needs to.

* *Sandboxes extend beyond just Haskell libraries*. As we saw above, by using
  `nix-shell` we sandboxed GHC itself. This means you can develop one package
  with GHC 7.4, another with GHC 7.6, and another with GHC HEAD. You don't
  really have to think about this, it will all Just Work. Furthermore, you can
  even depend on project specific software. One project at work requires I
  interact with a SQL Server instance, so I have `freetds` binaries available in
  that project, but I don't need to have them globally installed.

* *Nix helps me avoid Cabal hell.* The Haskell package repository inside Nix
  (`haskellPackages`) is a lot like Stackage. Rather than having every version
  available, we only have the latest version available (or very close to
  that). This makes things a *lot* simpler. Furthermore, we have a build farm
  that informs Nixpkgs maintainers when things don't work out, and we tend to
  push these issues upstream and resolve them as fast as we can. It's extremely
  rare that Cabal gets in the way for me these days.

## Conclusion

Since I installed NixOS last year, I haven't looked back. Developing Haskell
packages is a breeze, and I don't have to think about any of the more annoying
aspects that other people suffer with.

Everything I've mentioned above is available to you right now, without having to
commit to a full NixOS installation. Due to the purity, Nix is happy to sit
alongside other Linux distributions, so if you just want to try using Nix for a
single project there's almost no cost to do so.

The only cost Nix does come with is due to the size of our community. Sometimes
documentation may be a bit sparse, but I think we have a very active and helpful
IRC/mailing list. We're more than happy to help you and help Nix grow!

I hope this convinces you that Nix is far more than just a research project, and
has real practical benefits that have the potential to significantly improve
your work flow.
