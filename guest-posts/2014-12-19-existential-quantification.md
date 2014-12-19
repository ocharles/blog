---
title: 24 Days of GHC Extensions: Existential Quantification
---

I'm happy to announce that today [Roman Cheplyaka](http://ro-che.info/) is going
to take over 24 Days of GHC Extensions, and guide us through the very
interesting idea of *existential quantification*. Like
[yesterday](/guest-posts/2014-12-18-rank-n-types.html), this extension is much
more than mere syntax sugar - it changes the landscape as to how we can write
programs. Roman, the stage is yours!

---

Today we are going to look at existential types, also known as the
[`ExistentialQuantification`][10] extension. I won't explain the
theory of existential types here; it is admittedly somewhat complex, and good
books exist on the subject, to which I'll refer at the end of this post.

Instead, I want to show one small example of existential types, which hopefully
will make you interested enough to go and read those books!

Our example will be a HashMap module similar to [Data.HashMap][7]. Specifically,
what should an API for such a module look like?

We'll piggyback on the idea that Ollie described earlier in the series, in
the post about [Record Wildcards][9], namely: representing modules as records.
In that vein, let's represent our HashMap module as a record:

~~~ {.haskell}
data HashMap k v = ... -- actual implementation

data HashMapM = HashMapM
  { empty  :: forall k v . HashMap k v
  , lookup :: Hashable k => k -> HashMap k v -> Maybe v
  , insert :: Hashable k => k -> v -> HashMap k v -> HashMap k v
  , union  :: Hashable k => HashMap k v -> HashMap k v -> HashMap k v
  }
~~~

One advantage of doing so is the ability to parameterize this module on a
(possibly random) salt, which is important for [security reasons][8]. Instead of
having one static value of the `HashMapM` type, we'll have a function that takes
a salt and returns a record/module where each operation hashes keys based on
that salt:

~~~ {.haskell}
mkHashMapM :: Int -> HashMapM
~~~

Unfortunately, if we do that, bad things may happen.
Here's a recent example. Santa's junior elf, who only recently got into programming,
wanted to give Ollie a giraffe for Christmas, so he wrote this code:

~~~ {.haskell}
addGift :: HashMap Name Gift -> HashMap Name Gift
addGift gifts =
  let
    -- locally bring HashMapM functions into scope
    HashMapM{..} = mkHashMapM 42
  in
    insert "Ollie" giraffe gifts
~~~

The code compiled, and therefore it worked, or so the elf thought.

Later, when Santa looked up a gift for Ollie, he used his own
instantiation of `HashMapM` with a different salt, and the lookup turned up
Nothing. (Which maybe isn't that bad — keeping a giraffe ain't easy.)

Could we design the HashMap module to prevent such a rookie mistake?
Yes, with existential types!

First, we replace the concrete type `HashMap` with a type variable `hm` in the
record/module definition:

~~~ {.haskell}
data HashMapM hm = HashMapM
  { empty  :: forall k v . hm k v
  , lookup :: Hashable k => k -> hm k v -> Maybe v
  , insert :: Hashable k => k -> v -> hm k v -> hm k v
  , union  :: Hashable k => hm k v -> hm k v -> hm k v
  }
~~~

Next, we existentially quantify that `hm` variable by creating a wrapper:

~~~ {.haskell}
data HashMapE where
  HashMapE :: HashMapM hm -> HashMapE
~~~

Here I used the GADTs syntax, since it makes it easier to see what's going on.
When we wrap a module in the `HashMapE` constructor, we erase, or forget, the `hm`
type variable — notice how `hm` is not part of the result type. There's also
equivalent forall-syntax:

~~~ {.haskell}
data HashMapE = forall hm . HashMapE (HashMapM hm)
~~~

The only way to create `HashMapM` should be through this existential wrapper:

~~~ {.haskell}
-- public
mkHashMapE :: Int -> HashMapE
mkHashMapE = HashMapE . mkHashMapM

-- private
mkHashMapM :: Int -> HashMapM HashMap
mkHashMapM salt = HashMapM { {- implementation -} }
~~~

Now, the important thing about existential types is that every time we unpack
`HashMapE`, we get a fresh `hm` type. Operationally, the *implementation* of
`hm` is still `HashMap` (at least until we write another one, which we could
also pack into `HashMapE`), but from the type system's perspective, nothing
about `hm` is known.

Let's try again that elfin code (a variation of it, since we are not allowed to
use an existential pattern inside `let`; we need to use `case` instead):

~~~ {.haskell}
addGift :: HashMap Name Gift -> HashMap Name Gift
addGift gifts =
  case mkHashMapE 42 of
    HashMapE (HashMapM{..}) ->
      insert "Ollie" giraffe gifts
~~~

We'll get the following error:

~~~
    Couldn't match type ‘hm’ with ‘HashMap’
      ‘hm’ is a rigid type variable bound by
           a pattern with constructor
             HashMapE :: forall (hm :: * -> * -> *). HashMapM hm -> HashMapE,
           in a case alternative
    Expected type: HashMap Name Gift
      Actual type: hm Name Gift
~~~

(In fact, we shouldn't expose our implementation type `HashMap` at all; it's
now completely useless.)

But what if we replace `HashMap` with `hm`, just as the error message suggests?


~~~ {.haskell}
addGift :: hm Name Gift -> hm Name Gift
addGift gifts =
  case mkHashMapE 42 of
    HashMapE (HashMapM{..}) ->
      insert "Ollie" giraffe gifts
~~~

Still no luck:

~~~
    Couldn't match type ‘hm1’ with ‘hm’
      ‘hm1’ is a rigid type variable bound by
            a pattern with constructor
              HashMapE :: forall (hm :: * -> * -> *). HashMapM hm -> HashMapE,
            in a case alternative
      ‘hm’ is a rigid type variable bound by
           the type signature for addGift :: hm Name Gift -> hm Name Gift
    Expected type: hm Name Gift
      Actual type: hm1 Name Gift
~~~

The compiler is too clever to be tricked by our choice of names; it'll always
create a *fresh* type each type in unpacks `HashMapE`. So the elf has no choice but
to write code the right way, which is of course to take a module as an
argument:


~~~ {.haskell}
addGift :: HashMapM hm -> hm Name Gift -> hm Name Gift
addGift mod gifts =
  let
    HashMapM{..} = mod
  in
    insert "Ollie" giraffe gifts
~~~

Notice how in the type signature `hm` of the record/module is the same as `hm`
of the gift map. That makes the type checker happy.

And here's how Santa might use the function his elf has just written:

~~~ {.haskell}
sendGifts =
  case mkHashMapE santa'sSecretSalt of
    HashMapE (mod@HashMapM{..}) ->
      let
        gifts = addGift mod empty
      in
        traverse_ sendGiftToOllie $ lookup "Ollie" gifts
~~~

Unlike some other extensions, `ExistentialQuantification` wasn't introduced for some
specific purpose. Existential quantification is a concept from logic and type
theory which turned out to be quite useful in practice. Existential types help
model and implement:

* [abstract data types][2]
* ML-style packages
* object-oriented programming (see «Existential Objects» in [TAPL][3])
* [extensible exceptions][4]
* dynamic typing (as in `Data.Dynamic`)
* type-aligned sequences, such as [free applicative functors][5] or
  [bind chains][6]


If you plan to use existential types, I advise you to gain a deeper
understanding of them from a book on programming languages, such as Pierce's
[Types and Programming Languages][3] (recommended),
Mitchell's [Foundations for Programming Languages][11], or
[Practical Foundations for Programming Languages][12] (available [online][13] for
free).

----

*This post is part of
[24 Days of GHC Extensions](/pages/2014-12-01-24-days-of-ghc-extensions.html) -
for more posts like this, check out the
[calendar](/pages/2014-12-01-24-days-of-ghc-extensions.html)*.

[2]: https://dl.acm.org/citation.cfm?id=45065
[3]: http://www.cis.upenn.edu/~bcpierce/tapl/
[4]: http://community.haskell.org/~simonmar/papers/ext-exceptions.pdf
[5]: http://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors
[6]: http://homepages.cwi.nl/~ploeg/papers/zseq.pdf
[7]: http://hackage.haskell.org/package/unordered-containers-0.2.5.1/docs/Data-HashMap-Lazy.html
[8]: http://www.serpentine.com/blog/2012/12/13/a-major-new-release-of-the-haskell-hashable-library/
[9]: https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
[10]: https://downloads.haskell.org/~ghc/7.8.3/docs/html/users_guide/data-type-extensions.html#existential-quantification
[11]: https://mitpress.mit.edu/books/foundations-programming-languages
[12]: http://www.cambridge.org/us/knowledge/discountpromotion/?site_locale=en_US&code=L2PFPL
[13]: http://www.cs.cmu.edu/~rwh/plbook/book.pdf
