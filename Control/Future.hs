{-# LANGUAGE DeriveFunctor #-}
-- | Similar to async package, however, suitable for manual threading
-- and go without exceptions.
module Control.Future where
import Control.Applicative
import Control.Concurrent
import Data.IORef

data Progress a b = Making | Fixme a | Finished b
	deriving (Functor, Show)

-- | Two kinds of future is possible:
-- (i) A pile of failures [a] and (ii) Successful result b.
newtype Future a b = Future { runFuture :: IO (Progress [a] b) }

instance Functor (Future a) where
	fmap f (Future a) = Future $ (fmap.fmap) f a

instance Applicative (Future a) where
	pure = Future . return . Finished
	Future fs <*> Future as =
		Future $ do
			fs' <- fs
			as' <- as
			return $ case (fs', as') of
				(Finished f, Finished a) -> Finished $ f a
				(Fixme f, Finished _) -> Fixme f
				(Finished _, Fixme a) -> Fixme a
				(Fixme f, Fixme a) -> Fixme (f ++ a)
				_ -> Making

instance Alternative (Future a) where
	empty = Future $ return Making
	Future as <|> Future bs =
		Future $ do
			as' <- as
			case as' of
				Finished _ -> return as'
				_ -> bs

instance Monad (Future a) where
	return = pure
	Future m >>= f =
		Future $ do
			m' <- m
			case m' of
				Finished x -> runFuture (f x)
				Fixme l -> return (Fixme l)
				Making -> return Making

type Future' = Future String

-- | Wait until future comes, and modify failure history.
forceFuture :: Future a b -> ([a] -> IO b) -> IO b
forceFuture fu@(Future fs) f = do
	fs' <- fs
	case fs' of
		Finished r -> return r
		Fixme l -> f l
		Making -> threadDelay 1000 >> forceFuture fu f

-- | Just wait for the future honestly.
waitFuture :: Future a b -> IO (Progress [a] b)
waitFuture fu@(Future fs) = do
	fs' <- fs
	case fs' of
		Making -> threadDelay 1000 >> waitFuture fu
		otherwise -> return fs'

-- | Return 'Just' when it is time. The history may be modified.
maybeChance :: Future a b -> ([a] -> IO b) -> IO (Maybe b)
maybeChance (Future fs) f = do
	fs' <- fs
	case fs' of
		Finished r -> return $ Just r
		Fixme l -> f l >>= return . Just
		Making -> return Nothing

-- | If it is too early, immediately returns 'Making'.
eitherChance :: Future a b -> IO (Progress [a] b)
eitherChance (Future fs) = fs

-- | > asyncIO $ \update -> forkIO (doSth >>= update)
asyncIO :: ((Progress [a] b -> IO ()) -> IO ()) -> IO (Future a b)
asyncIO makeThread = do
	ref <- newIORef Making
	makeThread (writeIORef ref)
	return $ Future $ readIORef ref

-- | Run an action created in given 'Future' if it is available now.
runAction :: Future a (IO b) -> IO ()
runAction (Future fs) = do
	fs' <- fs
	case fs' of
		Finished run -> run >> return ()
		otherwise -> return ()
