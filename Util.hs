module Util where

import Control.Monad
import qualified Debug.Trace

maximaBy :: (a -> a -> Ordering) -> [a] -> [a]
maximaBy comp = go []
	where
		go accum [] 		= accum
		go [] (x:xs) 		= go [x] xs
		go as@(a:_) (x:xs) 	= case x `comp` a of
								GT -> go [x] 	xs
								EQ -> go (x:as) xs
								LT -> go as		xs

maximaByLength :: [[a]] -> [[a]]
maximaByLength = maximaBy compareByLen
	where
		compareByLen a b = length a `compare` length b

(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(.:) = (.) . (.)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

trace :: Show a => String -> a -> b -> b
trace comment a b =  Debug.Trace.trace (comment ++ ": " ++ show a) b

trace' :: Show a => String -> a -> a
trace' comment a =  Debug.Trace.trace (comment ++ ": " ++ show a) a

traceM :: (Show a, Monad m) => String -> m a -> m a
traceM comment = ((\p -> trace comment p (return p)) =<<)

maybeZero :: (MonadPlus m) => Maybe a -> m a
maybeZero = maybe mzero return
