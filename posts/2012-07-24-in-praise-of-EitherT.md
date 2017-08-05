---
title: "In Praise of EitherT"
---

At the start of the month, [Gabriel Gonzalez](http://www.haskellforall.com/)
released the [errors](http://hackage.haskell.org/package/errors) library for
Haskell. This library is fantastic, not only for what it provides, but also for
the learning it has motivated me to do. Amongst the various bits of Haskell
knowledge I am on a path to understand were the `EitherT`/`MaybeT` pair of monad
transformers. With a little bit of time, I've come to truly appreciate how
powerful these can be.

I'm currently working on an implementation of the
[OAuth2 specification for Snap](https://github.com/ocharles/snaplet-oauth2), and
there is a lot of outside-world interaction. Which means there's a lot of places
things can go wrong. Parsing request parameters, looking up things in databases,
checking for expiration times - these all have the possibility of failing, and
most of them require some form of IO too.

My code initially had the smell of walking indentation:

```haskell
grant' <- withBackend $ \be ->
            lookupAuthorizationGrant be (accessTokenCode tokenReq)
case grant' of
  Nothing -> -- handle error here
  Just grant ->
    case authGrantRedirectUri grant == accessTokenRedirect tokenReq of
      True -> do
        now <- liftIO getCurrentTime
        case now > authGrantExpiresAt grant of
          False -> do
            -- success, finally!
```

That's a brief snippet of code I used to have - 6 levels of nesting until we get
to the success case, where the real processing actually happens. But this code
is the important part, it should be the most prominent! Everything else is just
noise, and it'd be really nice if we could abstract that all away.

The `EitherT` monad transformer can do that. `EitherT` lets us lift actions from
some underlying monad, and add failure semantics. My first attempt at tidying
this code up was to run in `EitherT` and fail with a `Text` value, which could
then be rendered back to the client:

```haskell
grant' <- noteT "Authorization grant not found" .
            liftMaybe =<< liftIO =<< lift
               (withBackend' $ \be ->
                 inspectAuthorizationGrant be
                   (accessTokenReqCode tokenReq))

(authGrantRedirectUri grant == accessTokenReqRedirect tokenReq)
  `orFail` "Redirection URL must be the same"

now <- liftIO getCurrentTime
(now <= authGrantExpiresAt grant) `orFail` "This token has expired"

-- Success!
```

Now our success code is inline with the rest of the code, and not trailing off
the screen. Further more, the error handling has been simplified and is not
getting in the way. As good as that is, we've lost some functionality. For
example, we might need to send different status codes depending on what part of
validation failed, or in some cases of the OAuth specification we should change
the storage depending on failed requests (such as invalidating keys).

Here's the twist - we can make our error value an *executable action*. If we
change our error value to be `Handler b v ()`, we can run arbitrary code on
error conditions. Taking the example of expiration time:

```haskell
now <- liftIO getCurrentTime
(now <= authGrantExpiresAt grant) `orFail` expiredToken
...
where expiredToken = do writeText "This token has expired"
                        modifyResponse (setResponseCode 400)
```

Now we have a huge amount of flexibility on what can happen in chains of actions
that may fail. Further more, `where` definitions let us separate this code out
into small maintainable blocks. Finally, we can introduce functions such as
`orFail` to create what is almost a DSL for expressing rules in preparation for
later actions.

`EitherT` has helped me make my code avoid walking indentation, and closely ties
error handling to the code that could fail. I highly recommend you give
`EitherT` and `MaybeT` a chance if you haven't yet used them, if you just need
some lightweight and localised error handling, it might do the trick perfectly.
