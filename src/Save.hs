module Save where

import Utils
import Graphics
import System.Directory

clipboard = mkRef Nothing
saveFile = mkRef ""

initSave file = do
  saveFile $= file
  b <- doesFileExist file
  if b then read <$> readFile file else return []
