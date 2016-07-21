module Graphics.XHB.Monad
    ( X
    , unX
    , req
    , reqAsync
    , notify
    , IOU(..)
    , (<$>>)
    , (<*>>)
    ) where


import Graphics.XHB
import Graphics.XHB.Requests
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class


newtype X m a = X { runX :: ReaderT Connection (ExceptT SomeError m) a }

unX :: MonadIO m => X m a -> Connection -> m (Either SomeError a)
unX = fmap runExceptT . runReaderT . runX

instance MonadTrans X where

instance Functor m => Functor (X m) where

instance Applicative m => Applicative (X m) where

instance Monad m => Monad (X m) where


req :: (RequestWithReply a b, MonadIO m) => a -> X m b
req a = X . ReaderT $ \conn -> ExceptT . liftIO . join $ requestWithReply conn a

reqAsync :: (RequestWithReply a b, MonadIO m) => a -> X m (X m b)
reqAsync a = X . ReaderT $ \conn -> liftIO . fmap (X . lift . ExceptT . liftIO) $ requestWithReply conn a

notify :: (Request a, MonadIO m) => a -> X m ()
notify a = X . ReaderT $ \conn -> liftIO (request conn a)


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
