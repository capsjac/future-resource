{-# LANGUAGE DeriveFunctor #-}
-- | Similar to async package, however, suitable for manual threading
-- and go without exceptions.
module Control.Future where
import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Data.IORef
import Data.Monoid

data Progress a b = Making | Fixme a | Finished b
	deriving (Functor, Show)

-- | Two kinds of future is possible:
-- (i) A pile of failures (Monoid a) and (ii) Successful result b.
newtype Future a b = Future { runFuture :: IO (Progress a b) }

instance Functor (Future a) where
	fmap f (Future a) = Future $ (fmap.fmap) f a

instance Monoid a => Applicative (Future a) where
	pure = Future . return . Finished
	Future fs <*> Future as =
		Future $ do
			fs' <- fs
			as' <- as
			return $ case (fs', as') of
				(Finished f, Finished a) -> Finished $ f a
				(Fixme f, Finished _) -> Fixme f
				(Finished _, Fixme a) -> Fixme a
				(Fixme f, Fixme a) -> Fixme (f `mappend` a)
				_ -> Making

instance Monoid a => Alternative (Future a) where
	empty = Future $ return Making
	Future as <|> Future bs =
		Future $ do
			as' <- as
			case as' of
				Finished _ -> return as'
				_ -> bs

instance Monoid a => Monad (Future a) where
	return = pure
	Future m >>= f =
		Future $ do
			m' <- m
			case m' of
				Finished x -> runFuture (f x)
				Fixme l -> return (Fixme l)
				Making -> return Making

type Future' = Future [String]

-- | Wait until future comes, and modify failure history.
desire :: MonadIO m => Future a b -> (a -> IO b) -> m b
desire future@(Future f) fix = liftIO $ do
	prog <- f
	case prog of
		Finished result -> return result
		Fixme err -> fix err
		Making -> threadDelay 1000 >> desire future fix

-- | Just wait for the future honestly.
waitFor :: MonadIO m => Future a b -> m (Progress a b)
waitFor future@(Future f) = liftIO $ do
	prog <- f
	case prog of
		Making -> threadDelay 1000 >> waitFor future
		otherwise -> return prog

-- | Return 'Just' when it is time. The history may be modified.
maybeChance :: MonadIO m => Future a b -> (a -> IO b) -> m (Maybe b)
maybeChance (Future f) fix = liftIO $ do
	prog <- f
	case prog of
		Finished result -> return $ Just result
		Fixme err -> fix err >>= return . Just
		Making -> return Nothing

-- | If it is too early, immediately returns 'Making'.
getProgress :: MonadIO m => Future a b -> m (Progress a b)
getProgress (Future f) = liftIO f

-- | > mkFuture $ \updateProgress -> forkIO (doSth >>= updateProgress)
mkFuture :: MonadIO m => ((Progress a b -> IO ()) -> IO ()) -> m (Future a b)
mkFuture doFork = liftIO $ do
	progRef <- newIORef Making
	doFork (writeIORef progRef)
	return $ Future $ readIORef progRef

-- | Run 'Future' action immediately.
expect :: Show a => Future a b -> IO b
expect future =
	desire future (\a -> error $ "Control.Future.expect: " ++ show a)

