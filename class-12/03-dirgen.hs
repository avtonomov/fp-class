{-
  Для тестирования программ, работающих с файловой системой, часто необходимо
  достаточно большое дерево каталогов с файлами. Реализуйте случайный генератор
  такого дерева, управляемый набором параметров (минимальная и максимальная ширина
  и глубина дерева, количество и размеры файлов, что-нибудь ещё). В качестве идеи
  для архитектуры приложения используйте пример с подсчётом количества файлов в
  дереве (count.hs). Этот же пример можно использовать для тестирования
  разработанного приложения.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import System.Random
import System.IO.Unsafe
 
data AppConfig = AppConfig {
		min_depth_conf :: Int,
		max_depth_conf :: Int,
		min_width_conf :: Int,
		max_width_conf :: Int,  
		min_number_of_file :: Int,
		max_number_of_file :: Int,
		file_size_conf :: Int
     } deriving (Show)
 
data AppState = AppState {
	  stCurFileNum :: Int,
	  stCurDepth :: Int,
       stCurPath :: FilePath
     } deriving (Show)
 
newtype MyApp a = MyA {
      runA :: ReaderT AppConfig (StateT AppState IO) a
     } deriving (Functor, Applicative, Monad,
                 MonadIO,
                 MonadReader AppConfig,
                 MonadState AppState)
 
runMyApp :: MyApp a -> [Int] -> FilePath -> IO a
runMyApp app [min_depth, max_depth, min_width, max_width, min_file_num, max_file_num, file_size] path =
    let config = AppConfig min_depth max_depth min_width max_width min_file_num max_file_num file_size
        state = AppState 0 0 path
    in evalStateT (runReaderT (runA app) config) state
 
 	

random_string n flag
	| flag == 1 = do
				gen <- newStdGen
				return $ (take n $ randomRs ('a','z') gen) ++ ".txt"

	| otherwise = do
				gen <- newStdGen
				return $ take n $ randomRs ('a','z') gen

random_numb first second = do
	gen <- newStdGen
	return $ fst $ randomR (first, second) gen

	




create_files :: MyApp ()
create_files = do
	min_dep <-liftM min_depth_conf  ask
	max_dep <-liftM max_depth_conf  ask
	min_width <-liftM min_width_conf  ask
	max_width <-liftM max_width_conf  ask
	minFileNum <-liftM min_number_of_file  ask
	maxFileNum <-liftM max_number_of_file  ask
	
	purp_file_num <- liftIO $ random_numb minFileNum maxFileNum
	purp_depth <- liftIO $ random_numb min_dep max_dep
	purp_width <- liftIO $ random_numb min_width max_width

	file_size <-liftM file_size_conf  ask
	
	st <- get
	let cur_file_name = stCurFileNum st
	let cur_depth = stCurDepth st
	let cur_path = stCurPath st

	when (cur_depth < purp_depth) $ do
		forM_ [1..purp_file_num] $ \_ -> do
			file_name <- liftIO $ random_string 5 1
			text <-liftIO $ random_string 50 2
			let filePath = cur_path </> file_name
			liftIO $ writeFile filePath text
		
		forM_ [1..purp_width] $ \_ -> do
			dir_name <- liftIO $ random_string 5 0
			let newPath = cur_path </> dir_name
			liftIO $ createDirectory newPath
			let newDepth = cur_depth + 1
			put $ st {stCurDepth = newDepth, stCurPath = newPath, stCurFileNum = cur_file_name + purp_file_num}
			create_files

{- main -}
main = do
  (x : xs) <- getArgs		
  createDirectory x
  result <- runMyApp create_files (map read xs) x
  print result