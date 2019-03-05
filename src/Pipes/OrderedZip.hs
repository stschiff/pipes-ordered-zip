-- |A function to tie together two sorted Haskell Iterators
module Pipes.OrderedZip (orderedZip) where

import Pipes (Producer, next, yield, lift)

-- |orderedZip takes a comparison function and two producers and merges them 
-- together, creating a new Producer that yields pairs or Maybes of the two 
-- datatables provided by the two original producers
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
