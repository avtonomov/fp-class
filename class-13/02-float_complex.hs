import Parser
import SimpleParsers
import ParseNumbers
import Data.Maybe
import Control.Monad
import Control.Applicative hiding (many, optional)
import Data.Char

{- Напишите парсер для вещественных чисел. -}
float :: Parser Float
float = (*) <$> sign <*> (number <|> fromIntegral <$> natural)
  where
    number = do
      a <- natural
      char '.'
      b <- natural
      return $ fromIntegral a + (fromIntegral b / 10 ^ length_new b)
 
sign = (char '-' >> return (-1)) <|> return 1
length_new 0 = 0
length_new x = 1 + length_new (div x 10)
{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = bracket "("")" $ (,) <$> float <*> (char ',' >> (token float))

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[""]" $ sepBy (token complex) (symbol ";")

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[""]" $ sepBy (token complex <|> (liftM toComplex float)) (symbol ";")
   
toComplex a = (a, 0)
{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[""]" $ sepBy ( token complex <|> (liftM toComplex float)) (symbol ",")



