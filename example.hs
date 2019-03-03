import Pipes (runEffect, (>->), each)
import qualified Pipes.Prelude as P
import Pipes.OrderedZip (orderedZip)

main = do
    let a = each [1,3,4,6,8]
        b = each [2,3,4,5,8]
    let mergedProd = orderedZip compare a b
    _ <- runEffect $ mergedProd >-> P.print
    return ()
