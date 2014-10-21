{-
   Запрограммируйте игру «Быки и коровы» (https://ru.wikipedia.org/wiki/Быки_и_коровы)
   в варианте «компьютер загадывает — пользователь отгадывает».
-}

import System.Random
import System.IO
import System.Environment
import System.Directory
import Data.Char
import Control.Monad

import Data.List



gen_rand rand = (randomRs (1000, 9999) rand :: [Int])

game_play target = do 
	print "Write number"
	--print target
	number <- getLine
	let (cow,bull) = compare_ target number
	if bull == 4 
		then print "win"
        else do
                	print (cow,bull) 
                	game_play target

compare_  str1 str2 = (cows, bulls)
	where
    cows = (foldl (\acc c -> if elem c str1 then acc+1 else acc) 0 str2) - bulls
    bulls =length $ filter (==True) $ zipWith (==) str1 str2               	
   

main = do
	putStrLn "Lets play"
	rand <- newStdGen
	game_play $ show $ head( gen_rand rand)
