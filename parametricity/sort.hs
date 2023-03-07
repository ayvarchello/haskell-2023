import Control.Monad.ST (runST, ST)
import Data.Array.ST (STArray, newListArray)

makeArray :: [a] -> ST s (STArray s Int a)
makeArray xs = newListArray (0, length xs - 1) xs

sortArray :: STArray s Int a -> ST s ()
sortArray = do
  undefined

elems :: STArray s Int a -> ST s [a]
elems = undefined

sort :: Ord a => [a] -> [a]
sort xs = runST $ do
  arr <- makeArray xs
  sortArray arr
  elems arr

newtype RST e s a = RST { getRST :: e -> ST s a } deriving (Functor)

instance Applicative (RST e s)

instance Monad (RST e s)

runRST :: e -> (forall s. RST e s a) -> a
runRST env rst = runST (getRST rst env)
