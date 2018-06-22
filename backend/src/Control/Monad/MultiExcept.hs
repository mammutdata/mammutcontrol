{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MultiExcept
  ( MultiExceptT
  , runMultiExceptT
  , MonadMultiError(..)
  , throwErrors
  ) where

import Control.Monad.Base
import Control.Monad.Reader

import Data.Semigroup
import Data.List.NonEmpty

newtype MultiExceptT e m a
  = MultiExceptT { runMultiExceptT :: m (Either (NonEmpty e) a) }

instance Functor m => Functor (MultiExceptT e m) where
  fmap f = MultiExceptT . fmap (fmap f) . runMultiExceptT

instance Applicative m => Applicative (MultiExceptT e m) where
  pure = MultiExceptT . pure . Right

  ff <*> fx = MultiExceptT $ do
    ef <- runMultiExceptT ff
    ex <- runMultiExceptT fx
    pure $ case (ef, ex) of
      (Left es, Left es') -> Left $ es <> es'
      (Left es, _) -> Left es
      (_, Left es) -> Left es
      (Right f, Right x) -> Right $ f x

instance Monad m => Monad (MultiExceptT e m) where
  mx >>= f = MultiExceptT $ do
    eRes <- runMultiExceptT mx
    case eRes of
      Left es -> return $ Left es
      Right x -> runMultiExceptT $ f x

instance MonadTrans (MultiExceptT e) where
  lift = MultiExceptT . fmap Right

class Monad m => MonadMultiError e m where
  throwError :: e -> m a
  catchErrors :: m a -> (NonEmpty e -> m a) -> m a

instance Monad m => MonadMultiError e (MultiExceptT e m) where
  throwError = MultiExceptT . return . Left . (:| [])
  catchErrors action handler = MultiExceptT $
    runMultiExceptT action >>= runMultiExceptT . \case
      Left errs -> handler errs
      Right res -> return res

instance (Monad m, MonadMultiError e m) => MonadMultiError e (ReaderT r m) where
  throwError = lift . throwError
  catchErrors action handler = do
    r <- ask
    lift $ runReaderT action r `catchErrors` (flip runReaderT r . handler)

instance MonadBase b m => MonadBase b (MultiExceptT e m) where
  liftBase = liftBaseDefault

throwErrors :: (Applicative m, MonadMultiError e m) => NonEmpty e -> m a
throwErrors (e :| es) = case es of
  [] -> throwError e
  e':es' -> throwError e *> throwErrors (e' :| es')
