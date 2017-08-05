-----
title: "Working with postgresql-simple with generics-sop"
-----

The least interesting part of my job as a programmer is the act of pressing keys
on a keyboard, and thus I actively seek ways to reduce typing. As programmers,
we aim for reuse in a our programs - abstracting commonality into reusable
functions such that our programs get more concise. Functional programmers are
aware of the benefits of higher-order functions as one form of generic
programming, but another powerful technique is that of data type generic
programming.

This variant of generic programming allows one to build programs that work over
arbitrary data types, providing they have some sort of known "shape". We
describe the shape of data types by representing them via a code - often we can
describe a data type as a sum of products. By sum, we are talking about the
choice of a constructor in a data type (such as choosing between `Left` and
`Right` to construct `Either` values), and by product we mean the individual
fields in a constructor (such as the individual fields in a record).

Last month, [Edsko](http://www.edsko.net/) and [Löh](http://www.andres-loeh.de/)
announced a new library for generic programming:
[`generics-sop`](http://hackage.haskell.org/package/generics-sop). I've been
playing with this library in the last couple of days, and I absolutely love the
approach. In today's short post, I want to demonstrate how easy it is to use
this library. I don't plan to go into a lot of detail, but I encourage
interested readers to check out the associated paper -
[True Sums of Products](http://www.andres-loeh.de/TrueSumsOfProducts/) -
a paper with a lovely balance of theory and a plethora of examples.

## `postgresql-simple`

When working with `postgresql-simple`, one often defines records and
corresponding `FromRow` and `ToRow` instances. Let's assume we're modelling a
library. No library is complete without books, so we might begin with a record
such as:

```haskell
data Book = Book
  { bookTitle :: Text
  , bookAuthor :: Text
  , bookISBN :: ISBN
  , bookPublishYear :: Int
  }
```

In order to store and retrieve these in our database, we need to write the
following instances:

```haskell
instance FromRow Book where
  toRow = Book <$> field <*> field <*> field <*> field

instance ToRow Book where
  toRow Book{..} =
    [ toField bookTitle
    , toField bookAuthor
    , toField bookISBN
    , toField bookPublishYear
    ]
```

As you can see - that's a lot of boilerplate. In fact, it's nearly twice as much
code as the data type itself! The definitions of these instances are trivial, so
it's frustrating that I have to manually type the implementation bodies by
hand. It's here that we turn to `generics-sop`.

First, we're going to need a bit of boiler-plate in order to manipulate `Book`s
generically:

```haskell
data Book = ...
  deriving (GHC.Generics.Generic)

instance Generics.SOP.Generic Book
```

We derive generic representations of our `Book` using `GHC.Generics`, and in
turn use this generic representation to derive the `Generics.SOP.Generic`
instance. With this out of the way, we're ready to work with `Book`s in a
generic manner.

### `generics-sop`

The `generics-sop` library works by manipulating heterogeneous lists of data. If
we look at our `Book` data type, we can see that the following two are morally
describing the same data:

```haskell
book = Book "Conceptual Mathematics" "Lawvere, Schanuel" "978-0-521-71916-2" 2009
book = [ "Conceptual Mathematics", "Lawvere, Schanuel", "978-0-521-71916-2", 2009 ]
```

Of course, we can't actually write such a thing in Haskell - lists are required
to have all their elements of the same type. However, using modern GHC
extensions, we can get very close to modelling this:

```haskell
data HList :: [*] -> * where
  Nil :: HList '[]
  (:*) :: x -> HList xs -> HList (x ': xs)

book :: HList '[Text, Text, ISBN, Int]
book = "Conceptual Mathematics"
    :* "Lawvere, Schanuel"
    :* "978-0-521-71916-2"
    :* 2009
    :* Nil
```

Once we begin working in this domain, a lot of the techniques we're already
familiar with continue fairly naturally. We can map over these lists, exploit
their applicative functor-like structure, fold them, and so on.

`generics-sop` continues in the trend, using kind polymorphism and a few other
techniques to maximise generality. We can see what exactly is going on with
`generics-sop` if we ask GHCI for the `:kind!` of `Book`'s generic `Code`:

```
> :kind! Code Book
Code Book = SOP I '[ '[ Text, Text, ISBN, Int ] ]
```

The list of fields is contained within another list of all possible
constructors - as `Book` only has one constructor, thus there is only one
element in the outer list.

### `FromRow`, Generically

How does this help us solve the problem of our `FromRow` and `ToRow` instances?
First, let's think about what's happening when we write instances of
`FromRow`. Our `Book` data type has four fields, so we need to use `field` four
times. `field` has side effects in the `RowParser` functor, so we sequence all
of these calls using applicative syntax, finally applying the results to the
`Book` constructor.

Now that we've broken the problem down, we'll start by solving our first problem
- calling `field` the correct number of times. Calling `field` means we need to
have an instance of `FromField` for each field in a constructor, so to enforce
this, we can use `All` to require all fields have an instance of a type class.
We also use a little trick with `Proxy` to specify which type class we need to
use. We combine all of this with `hcpure`, which is a variant of `pure` that can
be used to build a product:

```
fields :: (All FromField xs, SingI xs) => NP RowParser xs
fields = hcpure fromField field
  where fromField = Proxy :: Proxy FromField
```

So far, we've built a product of `field` calls, which you can think of as being
a list of `RowParser`s - something akin to `[RowParser ..]`. However, we need a
single row parser returning multiple values, which is more like `RowParser
[..]`. In the `Prelude` we have a function to sequence a list of monadic actions:

```haskell
sequence :: Monad m => [m a] -> m [a]
```

There is an equivalent in `generics-sop` for working with heterogeneous lists -
`hsequence`. Thus if we `hsequence` our fields, we build a single `RowParser`
that returns a product of values:

```haskell
fields :: (All FromField xs, SingI xs) => RowParser (NP I xs)
fields = hsequence (hcpure fromField field)
  where fromField = Proxy :: Proxy FromField
```

(`I` is the "do nothing" identity functor).

Remarkably, these few lines of code are enough to construct data types. All we
need to do is embed this product in a constructor of a sum, and then switch from
the generic representation to a concrete data type. We'll restrict ourselves to
data types that have only one constructor, and this constraint is mentioned in
the type below (`Code a ~ '[ xs ]` forces `a` to have only one constructor):

```haskell
gfrowRow
  :: (All FromField xs, Code a ~ '[xs], SingI xs, Generic a)
  => RowParser a
gfrowRow = to . SOP . Z <$> hsequence (hcpure fromField field)
  where fromField = Proxy :: Proxy FromField
````

That's all there is to it! No type class instances, no skipping over meta-data -
we just build a list of `field` calls, `sequence` them, and turn the result into
our data type.

### `ToRow`, Generically

It's not hard to apply the same ideas for `ToRow`. Recall the definition of
`ToRow`:

```haskell
class ToRow a where
  toRow :: a -> [Action]
```

`toRow` takes a value of type `a` and turns it into a list of actions. Usually,
we have one action for each field - we just call `toField` on each field in the
record.

To work with data generically, we first need move from the original data type to
its generic representation, which we can do with `from` and a little bit of
pattern matching:

```haskell
gtoRow :: (Generic a, Code a ~ '[xs]) => a -> [Action]
gtoRow a =
  case from a of
    SOP (Z xs) -> _
```

Here we pattern match into the fields of the first constructor of the data
type. `xs` is now a product of all fields, and we can begin turning into
`Action`s. The most natural way to do this is simply to map `toField` over each
field, collecting the resulting `Action`s into a list. That is, we'd like to do:

```haskell
map toField xs
```

That's not quite possible in `generics-sop`, but we can get very close. Using
`hcliftA`, we can lift a method of a type class over a heterogeneous list:

```haskell
gtoRow :: (Generic a, Code a ~ '[xs], All ToField xs, SingI xs) => a -> [Action]
gtoRow a =
  case from a of
    SOP (Z xs) -> _ (hcliftA toFieldP (K . toField . unI) xs)

  where toFieldP = Proxy :: Proxy ToField
```

We unwrap from the identity functor `I`, call `toField` on the value, and then
pack this back up using the constant functor `K`. The details here are a little
subtle, but essentially this moves us from a heterogeneous list to a homogeneous
list, where each element of the list is an `Action`. Now that we have a
homogeneous list, we can switch back to a more basic representation by
collapsing the structure with `hcollapse`:

```haskell
gtoRow :: (Generic a, Code a ~ '[xs], All ToField xs, SingI xs) => a -> [Action]
gtoRow a =
  case from a of
    SOP (Z xs) -> hcollapse (hcliftA toFieldP (K . toField . unI) xs)

  where toFieldP = Proxy :: Proxy ToField
```

Admittedly this definition is a little more complicated than one might hope, but
it's still extremely concise and declarative - there's only a little bit of
noise added. However, again we should note there was no need to write type class
instances, perform explicit recursion or deal with meta-data - `generics-sop`
stayed out of way and gave us just what we needed.

### Conclusion

Now that we have `gfromRow` and `gtoRow` it's easy to extend our
application. Perhaps we now want to extend our database with `Author`
objects. We're now free to do so, with minimal boiler plate:

```haskell
data Book = Book
  { bookId :: Int
  , bookTitle :: Text
  , bookAuthorId :: Int
  , bookISBN :: ISBN
  , bookPublishYear :: Int
  } deriving (GHC.Generics.Generic)

instance Generic.SOP.Generic Book
instance FromRow Book where fromRow = gfromRow
instance ToRow Book where toRow = gtoRow

data Author = Author
  { authorId :: Int
  , authorName :: Text
  , authorCountry :: Country
  } deriving (GHC.Generics.Generic)

instance Generic.SOP.Generic Author
instance FromRow Author where fromRow = gfromRow
instance ToRow Author where toRow = gtoRow
```

`generics-sop` is a powerful library for dealing with data generically. By using
heterogeneous lists, the techniques we've learnt at the value level naturally
extend and we can begin to think of working with generic data in a declarative
manner. For me, this appeal to familiar techniques makes it easy to dive
straight in to writing generic functions - I've already spent time learning to
think in maps and folds, it's nice to see the ideas transfer to yet another
problem domain.

`generics-sop` goes a lot further than we've seen in this post. For more
real-world examples, see the links at the top of the
[`generics-sop`](http://hackage.haskell.org/package/generics-sop) Hackage page.
