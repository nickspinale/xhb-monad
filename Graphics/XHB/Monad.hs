{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.XHB.Monad
    ( X
    , unX

    , asksX

    , req
    , reqAsync
    , notify

    , IOU(..)
    , (<$>>)
    , (<*>>)
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


-- Class --


-- class Monad m => MonadX m where
--     liftX :: X n a -> m a
--     askX :: m Connection
--     catchErrorX :: m a -> (SomeError -> m a) -> m a

-- instance Monad m => MonadX (X m) where
--     liftX = 
--     askX = X $ ask
--     catchErrorX x f = X $ catchError (runX x) (runX . f)

-- -- TODO constraints
-- instance ( MonadX m
--          , MonadTrans t
--          , Monad (t m)
--          , MonadError SomeError (t (ReaderT Connection (ExceptT SomeError m)))
--          ) => MonadX (t m) where
-- -- instance forall m t n e. (MonadX m, MonadTrans t, Monad (t m), MonadError e n, MonadError e (t n)) => MonadX (t m) where
--     askX = lift askX
--     catchErrorX x f = catchError (lift $ runX x) (lift . runX . f)


-- Convenience --


toX :: (Connection -> m (Either SomeError a)) -> X m a
toX = X . ReaderT . fmap ExceptT


unX :: X m a -> Connection -> m (Either SomeError a)
unX = fmap runExceptT . runReaderT . runX


-- Communication --


req :: (RequestWithReply a b, MonadIO m) => a -> X m b
req a = X . ReaderT $ \conn -> ExceptT . liftIO . join $ requestWithReply conn a

reqAsync :: (RequestWithReply a b, MonadIO m) => a -> X m (X m b)
reqAsync a = X . ReaderT $ \conn -> liftIO . fmap (X . lift . ExceptT . liftIO) $ requestWithReply conn a

notify :: (Request a, MonadIO m) => a -> X m ()
notify a = X . ReaderT $ \conn -> liftIO (request conn a)


-- IOU --


newtype IOU m a = IOU { runIOU :: m (m a) }

instance Functor m => Functor (IOU m) where
    fmap f = IOU . fmap (fmap f) . runIOU

instance Monad m => Applicative (IOU m) where
    pure = IOU . pure . pure
    (IOU ma) <*> (IOU mb) = IOU $ (<*>) <$> ma <*> mb

(<$>>) :: (RequestWithReply a b, MonadIO m) => (b -> c) -> a -> X m (X m c)
f <$>> a = runIOU . fmap f . IOU $ reqAsync a

(<*>>) :: (RequestWithReply a b, MonadIO m) => X m (X m (b -> c)) -> a -> X m (X m c)
f <*>> a = runIOU $ IOU f <*> (IOU (reqAsync a))


-- MISC --


asksX :: Monad m => (Connection -> a) -> X m a
asksX = X . asks
