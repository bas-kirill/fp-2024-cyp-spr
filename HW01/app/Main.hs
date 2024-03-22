module Main (main) where

import Lib (ageOn)

main :: IO ()
main =
  do
    let r = 31557600 `ageOn` "Earth"
    print r