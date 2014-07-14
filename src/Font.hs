{-# LANGUAGE ImplicitParams, Rank2Types, NoMonomorphismRestriction #-}
{-| A module for fonts -}
module Font(withFont,fontSize) where

import Graphics
import Foreign.C.Types
import Paths_Epsilon

fontName = "resources/monofur.ttf"
withFont :: ((?font :: Font,?desc :: GLfloat,?asc :: GLfloat,?depth :: GLfloat
             ,?statusFont :: Font) => IO a) -> IO a
withFont m = do
  n <- getDataFileName fontName
  font <- createTextureFont n
  setFontFaceSize font 72 72
  statFont <- createPixmapFont n
  setFontFaceSize statFont 18 18
  -- setFontDepth font 5
  let ?font = font ; ?depth = 5
      ?desc = CFloat $ getFontDescender font
      ?asc = CFloat $ getFontAscender font
      ?statusFont = statFont
    in m
fontSize = ?desc + 72 + 5
