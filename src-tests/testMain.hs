import System.Exit (exitFailure)

import Control.Foldl (list, purely)
import Control.Monad (when)
import Pipes (each, Producer)
import Pipes.Prelude (fold')

import Pipes.OrderedZip (orderedZip)

main = do
    let a = each aVec
        b = each bVec
    let mergedProd = orderedZip compare a b
    (mergedList, _) <- purely fold' list mergedProd
    when (mergedList /= result) exitFailure
  where
    aVec = [1,3,4,6,8]
    bVec = [2,3,4,5,8]
    result = [(Just 1, Nothing),
              (Nothing, Just 2),
              (Just 3, Just 3),
              (Just 4, Just 4),
              (Nothing, Just 5),
              (Just 6, Nothing),
              (Just 8, Just 8)]

