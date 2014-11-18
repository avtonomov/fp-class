{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
import Control.Monad

intersect :: Eq a => [[a]] -> [a]
intersect [] = []
intersect (x:xs)= foldr (\acc temp -> return temp >>= filter (`elem`acc)) x xs
