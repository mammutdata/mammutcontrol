{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MultiExcept
  ( MultiExceptT
  , runMultiExceptT
  , MonadMultiError(..)
  , throwErrors
  , sequenceAll
  , sequenceAll_
  ) where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.State

import Data.List.NonEmpty

newtype MultiExceptT e m a
  = MultiExceptT { runMultiExceptT :: m (Either (NonEmpty e) a) }

instance Functor m => Functor (MultiExceptT e m) where
  fmap f = MultiExceptT . fmap (fmap f) . runMultiExceptT

instance Monad m => Applicative (MultiExceptT e m) where
  pure = MultiExceptT . pure . Right

  ff <*> fx = MultiExceptT $ do
    ef <- runMultiExceptT ff
    case ef of
      Left err -> pure $ Left err
      Right f -> do
        ex <- runMultiExceptT fx
        pure $ case ex of
          Left err -> Left err
          Right x  -> Right $ f x

instance Monad m => Monad (MultiExceptT e m) where
  mx >>= f = MultiExceptT $ do
    eRes <- runMultiExceptT mx
    case eRes of
      Left es -> return $ Left es
      Right x -> runMultiExceptT $ f x

instance MonadTrans (MultiExceptT e) where
  lift = MultiExceptT . fmap Right

instance MonadBase b m => MonadBase b (MultiExceptT e m) where
  liftBase = liftBaseDefault

instance MonadState s m => MonadState s (MultiExceptT e m) where
  state = lift . state

class Monad m => MonadMultiError e m | m -> e where
  throwError :: e -> m a
  catchErrors :: m a -> (NonEmpty e -> m a) -> m a

  -- | Similar to '<*>' but provides the additional guarantee that both
  -- actions will be evaluated.
  --
  -- This is useful so that we can ensure that, if the first action throws an
  -- exception, the second action will still be executed and both exceptions
  -- will be combined should there be a second one.
  --
  -- It is defined in a typeclass in order to define the same behaviour on any
  -- wrapping monad as well.
  apBoth :: m (a -> b) -> m a -> m b

instance (Monad m, Show e) => MonadMultiError e (MultiExceptT e m) where
  throwError = MultiExceptT . return . Left . (:| [])

  catchErrors action handler = MultiExceptT $
    runMultiExceptT action >>= runMultiExceptT . \case
      Left errs -> handler errs
      Right res -> return res

  apBoth ff fx = MultiExceptT $ do
    ef <- runMultiExceptT ff
    ex <- runMultiExceptT fx
    pure $ case (ef, ex) of
      (Left es, Left es') -> Left $ es <> es'
      (Left es, _) -> Left es
      (_, Left es) -> Left es
      (Right f, Right x) -> Right $ f x

instance (Monad m, MonadMultiError e m) => MonadMultiError e (ReaderT r m) where
  throwError = lift . throwError
  catchErrors action handler = do
    r <- ask
    lift $ runReaderT action r `catchErrors` (flip runReaderT r . handler)
  apBoth ff fx = ReaderT $ \r -> runReaderT ff r `apBoth` runReaderT fx r

instance (Monad m, MonadMultiError e m) => MonadMultiError e (StateT s m) where
  throwError = lift . throwError
  catchErrors action handler = do
    s <- get
    (x, s') <- lift $
      runStateT action s `catchErrors` (flip runStateT s . handler)
    put s'
    return x
  apBoth ff fx = ff <*> fx -- FIXME: this is just wrong!

throwErrors :: (Applicative m, MonadMultiError e m) => NonEmpty e -> m a
throwErrors (e :| es) = case es of
  [] -> throwError e
  e':es' -> throwError e `apBoth` throwErrors (e' :| es')

sequenceAll :: MonadMultiError e m => [m a] -> m [a]
sequenceAll =
  foldr (\action acc -> fmap (:) action `apBoth` acc) (pure [])

sequenceAll_ :: MonadMultiError e m => [m ()] -> m ()
sequenceAll_ =
  foldr (\action acc -> fmap (\_ _ -> ()) action `apBoth` acc) (pure ())
