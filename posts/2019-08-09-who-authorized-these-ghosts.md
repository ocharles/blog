---
title: Who Authorized These Ghosts!?
---

Recently at [CircuitHub](http://circuithub.com) we've been making some changes
to how we develop our APIs. We previously used
[Yesod](https://www.yesodweb.com/) with a custom router, but we're currently
exploring [Servant](https://hackage.haskell.org/package/servant) for API
modelling, in part due to it's potential for code generation for other clients
(e.g., our [Elm](http://elm-lang.org/) frontend). Along the way, this is
requiring us to rethink and reinvent previously established code, and one of
those areas is authorization.

To recap, [authorization is](https://en.wikipedia.org/wiki/Authorization)

> the function of specifying access rights/privileges to resources related to
> information security and computer security in general and to access control in
> particular.

This is in contrast to *authentication*, which is the act of showing that
someone is who they claim to be.

Authorization is a very important process, especially in a business like
CircuitHub where we host many confidential projects. Accidentally exposing this
data could be catastrophic to both our business and customers, so we take it
*very* seriously.

Out of the box, Servant has experimental support for authorization, which is a
good start. `servant-server` gives us
[`Servant.Server.Experimental.Auth`](https://hackage.haskell.org/package/servant-server-0.16.2/docs/Servant-Server-Experimental-Auth.html)
which makes it a doddle to plug in our existing authorization mechanism (cookies
& Redis). But that only shows that we know *who* is asking for resources, how do
we check that they are *allowed* to access the resources?

As a case study, I want to have a look at a particular end-point,
`/projects/:id/price`. This endpoint calculates the pricing options CircuitHub
can offer a project, and there are few important points to how this endpoint
works:

1. The pricing for a project depends on the user viewing it. This is because
   some users can consign parts so CircuitHub won't order them. Naturally, this
   affects the price, so pricing is viewer dependent.
2. Some projects are owned by organizations, and should be priced by the
   organization as a whole. If a user is a member of the organization that owns
   the project pricing has been requested for, return the pricing for the
   organization. If the user is not in the organization, return their own custom
   pricing.
3. Private projects should only expose their pricing to superusers, the owner of
   the project, and any members of the project's organization (if it's owned by
   an organization).

This specification is messy and complicated, but that's just reality doing it's
thing.

Our first approach was to try and represent this in Servant's API type. We start
with the "vanilla" route, with no authentication or authorization:

```haskell
type API = 
  "projects" 
    :> Capture "id" ProjectId 
    :> "price" 
    :> Get '[ JSON ] Pricing
```

Next, we add authorization:

```haskell
type API = 
  AuthProtect CircuitHub
    :> "projects"
    :> Capture "id" ProjectId 
    :> "price" 
    :> Get '[ JSON ] Pricing
```

At this point, we're on our own - Servant offers no authorization primitives
(though there are
[discussions](https://github.com/haskell-servant/servant-auth/issues/73) on this
topic).

My first attempt to add authorization to this was:

```haskell
type API = 
  AuthorizeWith ( AuthProtect CircuitHub )
    :> "projects"
    :> CanView ( Capture "id" ProjectId )
    :> "price" 
    :> Get '[ JSON ] Pricing
```

There are two new routing combinators here: `AuthorizeWith` and `CanView`. The
idea is `AuthorizeWith` somehow captures the result of authenticating, and
provides that information to `CanView`. `CanView` itself does some kind of
authorization using a type class based on its argument - here `Capture "id"
ProjectId`. The result is certainly something that worked, but I was unhappy
with both the complexity to implement it (which is scope to get it wrong), and
the lack of actual evidence of authorization.

The latter point needs some expanding. What I mean by "lacking evidence" is that
with the current approach, the authorization is essentially like writing the
following code:

```haskell
foo = do
  checkAuthorization
  doThings
```

If I later add more resource access into `doThings`, what will hold me
accountable to checking authorization on those resources? The answer is...
nothing! This is similar to [boolean
blindless](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/) -
we performed logical check, only to throw all the resulting evidence away
immediately.

At this point I wanted to start exploring some different options. While playing
around with ideas, I was reminded of the wonderful paper "[Ghosts of Departed
Proofs](https://kataskeue.com/gdp.pdf)", and it got me thinking... can we use
these techniques for authorization?

## Ghosts of Departed Proofs

The basic idea of GDP is to name values using higher-rank quantification, and
then - in trusted modules - produce proofs that refer to these names. To name
values, we introduce a `Named` type, and the higher-ranked function `name` to
name things:

```haskell
module Named ( Named, forgetName, name ) where

newtype Named n a = Named { forgetName :: a }

name :: a -> ( forall name. Named n a -> r ) -> r
name x f = f ( Named x )
```

Note that the *only* way to construct a `Named` value outside of this module is
to use `name`, which introduces a completely distinct name for a limited scope.
Within this scope, we can construct proofs that refer to these names. As a basic
example, we could use GDP to prove that a number is prime:

```haskell
module Prime ( IsPrime, checkPrime ) where

data IsPrime name = IsPrime

checkPrime :: Named name Int -> Maybe (IsPrime name)
checkPrime named | isPrime (forgetName named) = Just IsPrime
                 | otherwise                  = Nothing
```

Here we have our first proof witness - `IsPrime`. We can witness whether or not
a named `Int` is prime using `checkPrime` - like the boolean value `isPrime`
this determines if a number is or isn't prime, but we get evidence that we've
checked a *specific* value for primality.

This is the whirlwind tour of GDP, I highly recommend reading the paper for a
more thorough explanation. Also, the library
[`justified-containers`](https://hackage.haskell.org/package/justified-containers)
explores these ideas in the context of maps, where we have proofs that specific
items are in the map (giving us total lookups, rather than partial lookups).

## GDP and Authorization

This is all well and good, but how does this help with authorization? The basic
idea is that authorization is itself a proof - a proof that we can view or
interact with resources in a particular way. First, we have to decide which
functions need authorization - these functions will be modified to require proof
values the refer to the function arguments. In this example, we'll assume our
Servant handler is going to itself make a call to the `price :: ProjectId ->
UserId -> m Price` function. However, given the specification above, we need to
make sure that user and project are compatible. To do this, we'll name the
arguments, and then introduce a proof that the user in question can view the
project:

```haskell
price 
  :: Named projectId ProjectId 
  -> Named userId UserId 
  -> userId `CanViewProject` projectId 
  -> m Price
```

But what is this `CanViewProject` proof?

A first approximation is to treat it as some kind of primitive or axiom. A
blessed function can postulate this proof with no further evidence:

```haskell
module CanViewProject ( CanViewProject, canViewProject ) where

data CanViewProject userId projectId = 
  TrustMe

canViewProject 
  :: Named projectId ProjectId 
  -> Named userId UserId 
  -> m ( Maybe ( CanViewProject userId projectId ) )
canViewProject = do
  -- ... lots of database access/IO

  if ...
    then return ( Just TrustMe ) 
    else Nothing
```

This is a good start! Our `price` function can only be called with a
`CanViewProject` that matches the named arguments, and the only way to construct
such a value is to use `canViewProject`. Of course we could get the
implementation of *this* wrong, so we should focus our testing efforts to make
sure it's doing the right thing.

However, the Agda programmer in me is a little unhappy about just blindly
postulating `CanViewProject` at the end. We've got a bit of vision back from our
boolean blindness, but the landscape is still blurry. Fortunately, all we have
to do is recruit more of the same machinery so far to subdivide this proof into
smaller ones:

```haskell
module ProjectIsPublic ( ProjectIsPublic, projectIsPublic ) where

data ProjectIsPublic project = TrustMe

projectIsPublic 
  :: Named projectId ProjectId 
  -> m ( Maybe ( ProjectIsPublic projectId ) )
```

```haskell
module UserBelongsToProjectOrganization
  ( UserBelongsToProjectOrganization, userBelongsToProjectOrganization )
  where

data UserBelongsToProjectOrganization user project = TrustMe

userBelongsToProjectOrganization
  :: Named userId UserId
  -> Named projectId ProjectId
  -> m ( Maybe ( UserBelongsToProjectOrganization userId projectId ) )
```

```haskell
module UserIsSuperUser ( UserIsSuperUser, userIsSuperUser ) where

data UserIsSuperUser user = TrustMe

userIsSuperUser :: Named userId UserId -> m ( Maybe ( UserIsSuperUser userId ) )
```

```haskell
module UserOwnsProject ( UserOwnsProject, userOwnsProject ) where

data UserOwnsProject user project = TrustMe

userOwnsProject 
  :: Named userId UserId 
  -> Named projectId ProjectId 
  -> m ( Maybe ( UserOwnsProject userId projectId ) )
```

Armed with these smaller authorization primitives, we can build up our richer
authorization scheme:

```haskell
module CanViewProject where

data CanViewProject userId projectId
  = ProjectIsPublic (ProjectIsPublic projectId)
  | UserOwnsProject (UserOwnsProject userId projectId)
  | UserIsSuperUser (UserIsSuperUser userId)
  | UserBelongsToProjectOrganization 
      (UserBelongsToProjectOrganization userId projectId)

canViewProject
  :: Named userId UserId
  -> Named projectId ProjectId
  -> m ( Maybe ( CanViewProject userId projectId ) )
```

Now `canViewProject` just calls out to the other authorization routines to build
it's proof. Furthermore, there's something interesting here. `CanViewProject`
doesn't postulate anything - everything is attached with a proof of the
particular authorization case. This means that we can actually open up the whole
`CanViewProject` module to the world - there's no need to keep anything private.
By doing this and allowing people to pattern match on `CanViewProject`,
authorization results become reusable - if something else only cares that a user
is a super user, we might be able to pull this directly out of
`CanViewProject` - no need for any redundant database checks!

In fact, this very idea can help us implement the final part of our original
specification:

> Some projects are owned by organizations, and should be priced by the
> organization as a whole. If a user is a member of the organization that owns
> the project pricing has been requested for, return the pricing for the
> organization. If the user is not in the organization, return their own custom
> pricing.

If we refine our `UserBelongsToProjectOrganization` proof, we can actually
maintain a bit of extra evidence:

```haskell
data UserBelongsToProjectOrganization userId projectId where
  UserBelongsToProjectOrganization
    :: { projectOrganizationId :: Named orgId UserId
       , organizationOwnsProject :: UserOwnsProject orgId projectId
       }
    -> UserBelongsToProjectOrganization userId projectId

withUserBelongsToProjectOrganizationEvidence
  :: UserBelongsToProjectOrganization userId projectId
  -> ( forall orgId. Named orgId UserId -> UserOwnsProject orgId projectId -> r )
  -> r
withUserBelongsToProjectOrganizationEvidence UserBelongsToProjectOrganization{..} k =
  k projectOrganizationId organizationOwnsProject
```

Now whenever we have a proof `UserBelongsToProjectOrganization`, we can pluck
out the actual organization that we're talking about. We also have evidence that
the organization owns the project, so we can easily construct a new
`CanViewProject` proof - proofs generate more proofs!

```haskell
price 
  :: Named projectId ProjectId 
  -> Named userId UserId 
  -> userId `CanViewProject` projectId 
  -> m Price
price projectId userId = \case
  UserBelongsToProjectOrganization proof ->
    withUserBelongsToProjectOrganizationEvidence proof \orgId ownership ->
      price projectId orgId (UserOwnsProject ownership)
```

## Conclusion

That's where I've got so far. It's early days so far, but the approach is
promising. What I really like is there is almost a virtual slider between ease
and rigour. It can be easy to get carried away, naming absolutely everything and
trying to find the most fundamental proofs possible. I've found so far that it's
better to back off a little bit - are you *really* going to get some set
membership checks wrong? Maybe. But a property check is probably gonig to be
enough to keep that function in check. We're *not* in a formal proof engine
setting, pretending we are just makes things harder than they need to be.
