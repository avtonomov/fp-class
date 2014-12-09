{-
   Тип Parser может быть определён следуюшим образом:
-}
import Data.Maybe
import Control.Monad
import Data.Char
newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}


instance Monad Parser where
  return x = Parser (\s -> Just(x, s))
  p >>= q = Parser (\x_1 -> apply p x_1 >>= (\(x,x_2) -> apply (q x) x_2))
  fail _ = Parser (\_ -> Nothing)

instance MonadPlus Parser where
  mzero = Parser (\_ -> Nothing)
  p `mplus` q = Parser (\s -> let ps = apply p s in 
				if (isJust ps) then ps else apply q s)
