{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.XHB.Monad
    ( XContext(..)

    , X(..)
    , unX
    , toX

    , MonadX(..)
    , asksX
    , notify
    , reqAsync
    , req

    , WithReply(..)
    , withReply
    , (<$-)
    , (<*-)
    , doX

    ) where


import Graphics.XHB
import Graphics.XHB.Requests

import Data.Function
import Data.Typeable

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Control.Monad.Trans.Identity as Identity (IdentityT, liftCatch)
import Control.Monad.Trans.List as List (ListT, liftCatch)
import Control.Monad.Trans.Maybe as Maybe (MaybeT, liftCatch)
import Control.Monad.Trans.Reader as Reader (ReaderT, liftCatch)
import Control.Monad.Trans.State.Lazy as LazyState (StateT, liftCatch)
import Control.Monad.Trans.State.Strict as StrictState (StateT, liftCatch)
import Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT, liftCatch)
import Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT, liftCatch)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State


-- XContext --


class Monad x => XContext x where
    request :: Request a => a -> Connection -> x ()
    requestWithReply :: RequestWithReply a b => a -> Connection -> x (x (Either SomeError b))

instance XContext IO where
    request = requestIO
    requestWithReply = requestWithReplyIO


newtype WithReply x a = WithReply { runWithReply :: Connection -> x (x (Either SomeError a)) }

instance Functor x => Functor (WithReply x) where
    fmap f = WithReply . (fmap . fmap . fmap . fmap) f . runWithReply

instance Monad x => Applicative (WithReply x) where
    pure = WithReply . pure . pure . pure . pure
    -- f <*> a = WithReply $ (liftA2 . liftA2 . liftA2) (<*>) (runWithReply f) (runWithReply a)
    f <*> a = WithReply $ \conn -> do
                xf <- runWithReply f conn
                xa <- runWithReply a conn
                return . runExceptT $ ExceptT xf <*> ExceptT xa


withReply :: (XContext x, RequestWithReply a b) => a -> WithReply x b
withReply = WithReply . requestWithReply

infixl 5 <$-, <*-

(<$-) :: (XContext x, RequestWithReply a b) => (b -> c) -> a -> WithReply x c
f <$- a = f <$> withReply a

(<*-) :: (XContext x, RequestWithReply a b) => WithReply x (b -> c) -> a -> WithReply x c
f <*- a = f <*> withReply a

doX :: MonadX x m => WithReply x a -> m a
doX x = do
    conn <- askX
    liftX (join (runWithReply x conn)) >>= either throwErrorX return


-- X --


newtype X m a = X { runX :: ReaderT Connection (ExceptT SomeError m) a }
    deriving (Functor, Applicative, Monad, MonadIO, Typeable)


instance MonadTrans X where
    lift = X . lift . lift


toX :: (Connection -> m (Either SomeError a)) -> X m a
toX = X . ReaderT . fmap ExceptT


unX :: X m a -> Connection -> m (Either SomeError a)
unX = fmap runExceptT . runReaderT . runX


-- MonadX --


class (XContext x, Monad m) => MonadX x m | m -> x where
    liftX :: x a -> m a
    askX :: m Connection
    catchErrorX :: m a -> (SomeError -> m a) -> m a
    throwErrorX :: SomeError -> m a


instance XContext x => MonadX x (X x) where
    liftX = X . lift . lift
    askX = X ask
    catchErrorX m f = X $ catchError (runX m) (runX . f)
    throwErrorX = X . throwError


asksX :: MonadX x m => (Connection -> a) -> m a
asksX = flip fmap askX


notify :: (MonadX x m, Request a) => a -> m ()
notify a = askX >>= (liftX . request a)


reqAsync :: (MonadX x m, RequestWithReply a b) => a -> m (m b)
reqAsync a = do
    conn <- askX
    x <- liftX $ requestWithReply a conn
    either throwErrorX return <$> liftX x


req :: (MonadX x m, RequestWithReply a b) => a -> m b
req a = do
    conn <- askX
    liftX (join (requestWithReply a conn)) >>= either throwErrorX return


-- X mtl instances --


instance MonadError e m => MonadError e (X m) where
    throwError = lift . throwError
    catchError x f = toX $ \conn -> catchError (unX x conn) (flip ($) conn . unX . f)

instance MonadReader r m => MonadReader r (X m) where
    ask = lift ask
    local f x = toX $ \conn -> local f (unX x conn)

instance MonadState s m => MonadState s (X m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (X m) where
    tell = lift . tell
    listen = X . listen . runX
    pass = X . pass . runX


-- MonadX mtl instances --


instance MonadX x m => MonadX x (ExceptT e m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX m f = ExceptT $ catchErrorX (runExceptT m) (runExceptT . f)

--

instance MonadX x m => MonadX x (IdentityT m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX = Identity.liftCatch catchErrorX

instance MonadX x m => MonadX x (ListT m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX = List.liftCatch catchErrorX

instance MonadX x m => MonadX x (MaybeT m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX = Maybe.liftCatch catchErrorX

instance MonadX x m => MonadX x (ReaderT r m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX = Reader.liftCatch catchErrorX

instance MonadX x m => MonadX x (LazyState.StateT s m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX = LazyState.liftCatch catchErrorX

instance MonadX x m => MonadX x (StrictState.StateT s m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX = StrictState.liftCatch catchErrorX

instance (Monoid w, MonadX x m) => MonadX x (LazyWriter.WriterT w m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX = LazyWriter.liftCatch catchErrorX

instance (Monoid w, MonadX x m) => MonadX x (StrictWriter.WriterT w m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX = StrictWriter.liftCatch catchErrorX
