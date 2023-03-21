import Control.Monad.ST (ST)
import Control.Monad.ST qualified as ST
import Prelude hiding (uncurry)

uncurry :: forall a b c. (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

runST :: (forall s. ST s a) -> a
runST = ST.runST

data ShowObject = forall a. Show a => ShowObject a Int

mkShow :: Show a => a -> ShowObject
mkShow = (`ShowObject` 0)

consume :: (forall a. Show a => a -> b) -> ShowObject -> b
consume f (ShowObject x _) = f x

apples = [mkShow 1, mkShow "apples"]

showAll :: [ShowObject] -> String
showAll xs = unwords [consume show x | x <- xs]

main = putStrLn (showAll apples)

class InputStream a where
  readS :: a -> IO Char

class OutputStream a where
  write :: Char -> a -> IO ()

type IOStream a = (InputStream a, OutputStream a)

io :: IOStream a => a -> IO ()
io a = readS a >>= (`write` a)

data InputObject = forall a. InputStream a => InputObject a

instance InputStream InputObject where
  readS (InputObject x) = readS x

data OutputObject = forall a. OutputStream a => OutputObject a

instance OutputStream OutputObject where
  write c (OutputObject x) = write c x

data IOObject = forall a. IOStream a => IOObject a

instance InputStream IOObject where
  readS (IOObject x) = readS x

instance OutputStream IOObject where
  write c (IOObject x) = write c x

-- class (Functor f, Functor g) => f :<: g where
--    inj :: f a -> g a

class x :<: y where
  wrap :: x -> y
