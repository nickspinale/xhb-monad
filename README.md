# xhb-monad

This package contains some monads providing a uniform interface to X11 via [xhb](https://hackage.haskell.org/package/xhb) and [xhb-requests](https://github.com/nspin/xhb-requests).
At its core are the following two classes:

```
import Graphics.XHB.Requests

class Monad x => XContext x where
    request :: Request a => a -> Connection -> x ()
    requestWithReply :: RequestWithReply a b => a -> Connection -> x (x (Either SomeError b))
    waitEvent :: Connection -> x SomeEvent
    pollEvent :: Connection -> x (Maybe SomeEvent)

class (XContext x, Monad m) => MonadX x m | m -> x where
    liftX :: x a -> m a
    askX :: m Connection
    catchErrorX :: m a -> (SomeError -> m a) -> m a
    throwErrorX :: SomeError -> m a
```

In addition, it includes an `instance XContext IO`, monad transformer instanaces, and some convenient types and operations.

Its purpose is to allow for *pure* X logic.

## Documentation

[This article](https://nickspinale.com/articles/pure-x11-logic-in-haskell.html) describes this package and some of its friends in detail.

Haddock can be found [here](https://nspin.github.io/xhb-monad).
