{-# LANGUAGE ImplicitParams, Rank2Types, NoMonomorphismRestriction #-}
module Font where

import Graphics
import Foreign.C.Types
import Paths_Epsilon

fontName = "resources/monofur.ttf"
withFont :: ((?font :: Font,?desc :: GLfloat,?asc :: GLfloat,?depth :: GLfloat) => IO a) -> IO a
withFont m = do
  font <- createTextureFont =<< getDataFileName fontName
  setFontFaceSize font 72 50
  -- setFontDepth font 5
  let ?font = font ; ?depth = 5
      ?desc = CFloat $ getFontDescender font
      ?asc = CFloat $ getFontAscender font
    in m
fontSize = ?desc + 72 + 5
