module Control.Future where
import Control.Applicative
import Control.Concurrent

-- | Two kinds of future is possible:
-- (i) A pile of failures [a] and (ii) Successful result b.
newtype Future a b = Future { runFuture :: IO (Maybe (Either [a] b)) }

instance Functor (Future a) where
	fmap f (Future a) = Future $ (fmap.fmap.fmap) f a

instance Applicative (Future a) where
	pure = Future . return . Just . Right
	Future fs <*> Future as =
		Future $ do
			fs' <- fs
			as' <- as
			return $ case (fs', as') of
				(Just f, Just a) -> Just $ case (f, a) of
					(Right f, Right a) -> Right $ f a
					(Left f, Right _) -> Left f
					(Right _, Left a) -> Left a
					(Left f, Left a) -> Left (f ++ a)
				_ -> Nothing

instance Alternative (Future a) where
	empty = Future $ return Nothing
	Future as <|> Future bs =
		Future $ do
			as' <- as
			case as' of
				Just (Right _) -> return as'
				_ -> bs

instance Monad (Future a) where
	return = pure
	Future m >>= f =
		Future $ do
			m' <- m
			case m' of
				Just (Right x) -> runFuture (f x)
				Just (Left l) -> return (Just (Left l))
				Nothing -> return Nothing

-- | Wait until future comes, and modify failure history.
forceFuture :: Future a b -> ([a] -> IO b) -> IO b
forceFuture fu@(Future fs) f = do
	fs' <- fs
	case fs' of
		Just (Right r) -> return r
		Just (Left l) -> f l
		Nothing -> threadDelay 1000 >> forceFuture fu f

-- | Just wait for the future honestly.
waitFuture :: Future a b -> IO (Either [a] b)
waitFuture fu@(Future fs) = do
	fs' <- fs
	case fs' of
		Just e -> return e
		Nothing -> threadDelay 1000 >> waitFuture fu

-- | Return 'Just' when it is time. The history may be modified.
maybeChance :: Future a b -> ([a] -> IO b) -> IO (Maybe b)
maybeChance (Future fs) f = do
	fs' <- fs
	case fs' of
		Just (Right r) -> return $ Just r
		Just (Left l) -> fmap Just (f l)
		Nothing -> return Nothing

-- | If it is too early, immediately returns 'Nothing'.
eitherChance :: Future a b -> IO (Maybe (Either [a] b))
eitherChance (Future fs) = fs

