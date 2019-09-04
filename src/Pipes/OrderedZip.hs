-- |A function to tie together two sorted Haskell Iterators
module Pipes.OrderedZip (orderedZip, orderCheckPipe, WrongInputOrderException(..)) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Pipes (Producer, next, yield, lift, Pipe, for, cat)
import Pipes.Safe (MonadSafe, throwM)

-- |orderedZip takes a comparison function and two producers and merges them 
-- together, creating a new Producer that yields pairs of Maybes of the two 
-- datatables provided by the two original producers.
-- The output pairs reflect the Union of the two input producers, with Nothings indicating
-- missing data in one of the producers at that location.
orderedZip :: (Monad m) => (a -> b -> Ordering) -- ^The function to compare types of a with b
           -> Producer a m r1 -- ^The first producer (has to be ordered)
           -> Producer b m r2 -- ^The second producer (has to be ordered)
           -> Producer (Maybe a, Maybe b) m (r1, r2) -- ^The merged producer
orderedZip ord p1 p2 = do
    p1Front <- lift $ next p1
    p2Front <- lift $ next p2
    go ord p1Front p1 p2Front p2
    where
        go ord' p1Front p1' p2Front p2' = case (p1Front, p2Front) of
            (Left p1r, Left p2r) -> return (p1r, p2r)
            (Left _, Right (p2a, p2Rest)) -> do
                yield (Nothing, Just p2a)
                p2Front' <- lift $ next p2Rest
                go ord' p1Front p1' p2Front' p2Rest
            (Right (p1a, p1Rest), Left _) -> do
                yield (Just p1a, Nothing)
                p1Front' <- lift $ next p1Rest
                go ord' p1Front' p1Rest p2Front p2'
            (Right (p1a, p1Rest), Right (p2a, p2Rest)) -> case ord p1a p2a of
                LT -> do
                    yield (Just p1a, Nothing)
                    p1Front' <- lift $ next p1Rest
                    go ord' p1Front' p1Rest p2Front p2'
                EQ -> do
                    yield (Just p1a, Just p2a)
                    p1Front' <- lift $ next p1Rest
                    p2Front' <- lift $ next p2Rest
                    go ord' p1Front' p1Rest p2Front' p2Rest
                GT -> do
                    yield (Nothing, Just p2a)
                    p2Front' <- lift $ next p2Rest
                    go ord' p1Front p1' p2Front' p2Rest

-- an exception type to represent invalid input order
data WrongInputOrderException = WrongInputOrderException String deriving (Show, Eq)
instance Exception WrongInputOrderException

-- a pipe to check wether the stream is ordered according to a custom ordering function
orderCheckPipe :: (MonadIO m, MonadSafe m, Show a) => (a -> a -> Ordering) -- ^the custom ordering function
               -> Pipe a a m r -- ^the resulting pipe
orderCheckPipe cmpFunc = do
    lastValRef <- liftIO $ newIORef (Nothing)
    for cat $ \entry -> do
        lastVal <- liftIO $ readIORef lastValRef
        case lastVal of
            Nothing -> return ()
            Just p -> case cmpFunc entry p of
                LT -> throwM $ WrongInputOrderException ("ordering violated: " ++ show p ++ " should come after " ++ show entry)
                _ -> return ()
        liftIO $ writeIORef lastValRef (Just entry)
        yield entry
