import System.Environment
import Data.Monoid
import Data.Maybe

import Control.Applicative
import System.Random
import System.IO
import System.Directory
import Data.Char

{-
  Некоторый датчик генерирует по пять сигналов в сутки, часть из которых
  не доходит до базовой станции. Полученные от датчика сведения представлены
  текстовым файлом, содержащим по одному целому числу в каждом строке. Если
  сигнал не был получен, вместо числа в файле записывается прочерк (символ '-').
-}

type SensorValue = Maybe Int
type SensorData = [SensorValue]

{- Напишите функцию, которая преобразует прочитанную из файла строку в список
   значений, полученных от датчика. -}



getData :: String -> SensorData
getData = fmap (\x -> if x=="-" then Nothing else Just (read x)) . lines

{- Напишите функцию, группирующую данные по суткам. -}

dataByDay :: SensorData -> [SensorData]
dataByDay [] = []
dataByDay xs = (take 5 xs): dataByDay (drop 5 xs)



{-
  Посчитайте минимальное значение среди показаний датчика,
  полученных:
  а) первыми в течение суток;
  б) последними в течение суток.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов First и Last,
  при этом должна быть написана одна функция, отвечающая на вопрос а) или б)
  в зависимости от значения логического параметра.
-}

minData1 :: Bool -> [SensorData] -> Int
minData1 needFirst xs 
   |needFirst ==True = fromJust $ foldl1 minApp $ map (getFirst . mconcat . map First) xs
   |otherwise = fromJust $ foldl1 maxApp2 $ map (getLast . mconcat . map Last) xs


maxApp2 a b= max <$> a <*> b 
minApp a b= min <$> a <*> b 


{-
  Посчитайте минимальное значение среди данных,
  полученных:
  а) как суммы всех показаний датчика за каждые сутки;
  б) как произведения всех показаний датчика за каждые сутки.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов Sum, Product
  и Maybe a, где a — моноид, при этом должна быть написана одна функция, отвечающая
  на вопрос а) или б) в зависимости от значения логического параметра.

   

-}

minData2 :: Bool -> [SensorData] -> Int
minData2 needSum  xs
   |needSum ==True = minimum $ map (getSum . mconcat . map (Sum . fromJust) . filter isJust) xs
   |otherwise = minimum $ map (getProduct . mconcat . map (Product . fromJust) . filter isJust) xs


{- Попробуйте объединить две предыдущие функции в одну. -}

data SensorTask = NeedFirst | NeedLast | NeedSum | NeedProduct

minData :: SensorTask -> [SensorData] -> Int
minData NeedFirst xs = minData1 True xs
minData NeedLast xs = minData1 False xs
minData NeedSum xs = minData2 True xs
minData NeedProduct xs = minData2 False xs 

{-
  Пользуясь моноидами All, Any и любыми другими, выясните следующую информацию:
  1) количество суток, за которые не было получено ни одного показания;
  2) количество суток, показания за которые получены полностью;
  3) количество суток, за которые было получено хотя бы одно показание;
  4) количество суток, сумма показаний за которые превосходит заданное число;
  5) количество суток, произведение показаний за которые превосходит заданное число;
  6) количество суток, первое показание за которые превосходит заданное число;
  7) количество суток, последнее показание за которые превосходит заданное число.

  Постарайтесь ответить на все вопросы, написав одну функцию.
-}

main = do
  fname <- head `fmap` getArgs
  sData <- getData `fmap` readFile fname
  print $ minData1 True $dataByDay sData
  print $ minData2 True $dataByDay sData
