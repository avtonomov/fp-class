{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}


{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.
-}


class Listable a
	where
		toList :: a -> [a]
		fromList :: [a] -> a

instance Listable String 
	where
		toList = words
		fromList = unwords

instance Listable Integer 
	where
		toList a = reverse $ temp_fun a
			where
				temp_fun 0 = []
				temp_fun a = a `mod` 10 : (temp_fun (a `div` 10))
		fromList = foldl (\acc temp -> acc * 10+temp) 0 				