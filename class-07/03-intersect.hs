{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Random
import System.IO
import System.Environment
import System.Directory
import Data.Char
import qualified Data.IntSet as Set


readNumFile ::FilePath -> IO [Int]
readNumFile file = do
    handle <- openFile file ReadMode
    contents <- readFile file
    return (map read $ concatMap words $ lines contents)

solve ::[[Int]] -> (Int, [Int])
solve list = (length set, set)
	where 
		set =Set.toList ( foldl1 Set.intersection (map Set.fromList list))


main = getArgs >>= mapM readNumFile >>= print.solve
