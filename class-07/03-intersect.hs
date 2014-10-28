{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Environment
import System.IO
import System.Environment
import System.Directory
import Data.Char
import qualified Data.IntSet as Set

readNumFile :: FilePath -> IO [Int]
readNumFile file = do
	handle <- openFile file ReadMode
	content <- hGetContents handle 
	return $ map read $ concatMap words $ lines content

solve ::[[Int]] -> (Int, [Int])
solve xs = (length temp, temp)
	where
		temp = Set.toList $ foldl1 Set.intersection $ map Set.fromList xs

main = getArgs >>= mapM readNumFile >>= print.solve
