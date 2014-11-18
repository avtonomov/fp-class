import Control.Monad
import Data.List
import System.Environment
import Data.Maybe

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

kinds = [Chitin, Hide, Leather, Elven, Scaled, Glass, ImperialLight]
read_item [armor_kind, armor_type] = ArmorItem (read armor_kind) (read armor_type)

loadInventory :: FilePath -> IO [ArmorItem]
loadInventory fname = readFile fname >>= return . map (read_item . words) . lines

		


buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kind xs
    | length temp == 5 = Just (ArmorKit kind temp)
	| otherwise = Nothing
	where
		temp = foldl (\acc (ArmorItem kind_ x) -> if kind_ == kind then x:acc else acc ) [] xs

buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits armor_items = sequence $ filter (isJust) $ zipWith buildArmorKit kinds (replicate 7 armor_items)

main = (head `liftM` getArgs) >>= loadInventory  >>= print.buildKits
