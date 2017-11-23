module Main where

import Lucid.Base
import Stinky

main :: IO ()
main = do
  print $ renderText (row_ "a")
  print $ renderText (columns_ "")
  print $ renderText (row_ $ columns_ "")
  print $ renderText $ with columns_ [class_ "abc"] "asdf"
  print $ renderText $ with p_ [class_ "brand"] "Lucid Inc"
