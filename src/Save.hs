-- |A module describing 
module Save(saveFile,initSave,writeSaveFile) where

import Utils
import Graphics
import System.Directory
import Model(trees)

-- |The name of the save file
saveFile = mkRef ""

-- |Sets the name of the save file and try to read its content
initSave file = do
  saveFile $= file
  b <- doesFileExist file
  if b then read <$> readFile file else return []
-- |Writes the current trees to the save file
writeSaveFile = join (liftA2 writeFile (get saveFile) (show <$> get trees))
