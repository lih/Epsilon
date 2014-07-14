module Wheel where

import Trees
import Utils

data Wheel = Wheel (Maybe Filepath) [IORef (Syntax String)]

