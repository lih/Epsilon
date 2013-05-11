module Save where

import Utils
import Graphics
import System.Directory
import Model(expr)

saveFile = mkRef ""

initSave file = do
  saveFile $= file
  b <- doesFileExist file
  if b then read <$> readFile file else return []
writeSaveFile = join (liftA2 writeFile (get saveFile) (show <$> get expr))
