import Pipes (runEffect, (>->), each)
import qualified Pipes.Prelude as P
import Pipes.OrderedZip (orderedZipAll)

main = do
    let a = each ([1,  3,4,  6,  8] :: [Int])
        b = each ([  2,3,4,5,    8] :: [Int])
        c = each ([  2,3,  5,  7,8] :: [Int])
        mergedProd = orderedZipAll compare [a, b, c]
    _ <- runEffect $ mergedProd >-> P.print
    return ()


    