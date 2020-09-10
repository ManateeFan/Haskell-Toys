module Codewars.Kata.XO where

import Data.Char (toLower)
import Data.List (filter)

xo :: String -> Bool
xo str = count 'x' str == count 'o' str
  where count char = length . filter ((==) char . toLower)