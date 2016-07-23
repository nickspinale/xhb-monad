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
    ( XContext

    , X(..)
    , unX
    , toX

    , MonadX(..)

    , asksX

    , notify
    , reqAsync
    , req

    , (<$-)
    , (<*-)
    , doX

    , IOU(..)
    ) where


import Graphics.XHB
import Graphics.XHB.Requests

import Data.Constraint
import Data.Typeable
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class


-- XContext --


class Monad m => XContext m where
    request :: Request a => Connection -> a -> m ()
    requestWithReply :: RequestWithReply a b => Connection -> a -> m (m (Either SomeError b))

instance XContext IO where
    request = requestIO
    requestWithReply = requestWithReplyIO


-- X --


newtype X m a = X { runX :: ReaderT Connection (ExceptT SomeError m) a }
  deriving (Functor, Applicative, Monad, MonadIO, Typeable)

instance MonadTrans X where
    lift = X . lift . lift

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
notify a = do
    conn <- askX
    liftX $ request conn a


reqAsync :: (MonadX x m, RequestWithReply a b) => a -> m (m b)
reqAsync a = do
    conn <- askX
    x <- liftX $ requestWithReply conn a
    either throwErrorX return <$> liftX x 


req :: (MonadX x m, RequestWithReply a b) => a -> m b
req a = do
    conn <- askX
    liftX (join (requestWithReply conn a)) >>= either throwErrorX return


(<$-) :: (XContext x, RequestWithReply a b) => (b -> c) -> a -> Connection -> x (x (Either SomeError c))
(f <$- a) conn = (fmap.fmap.fmap) f $ requestWithReply conn a


(<*-) :: (XContext x, RequestWithReply a b) => (Connection -> x (x (Either SomeError (b -> c)))) -> a -> Connection -> x (x (Either SomeError c))
(mmf <*- a) conn = do
    mf <- mmf conn
    mb <- requestWithReply conn a
    return . runExceptT $ ExceptT mf <*> ExceptT mb


doX :: MonadX x m => (Connection -> x (x (Either SomeError a))) -> m a
doX x = do
    conn <- askX
    liftX (join (x conn)) >>= either throwErrorX return


-- IOU --


newtype IOU m a = IOU { runIOU :: m (m a) }

instance Functor m => Functor (IOU m) where
    fmap f = IOU . fmap (fmap f) . runIOU

instance Monad m => Applicative (IOU m) where
    pure = IOU . pure . pure
    (IOU ma) <*> (IOU mb) = IOU $ (<*>) <$> ma <*> mb


-- Lame instances --

-- TODO
