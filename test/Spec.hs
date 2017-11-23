{-# LANGUAGE OverloadedStrings #-}
import Lucid.Base
import Stinky
import Test.Hspec

main :: IO ()
main = hspec $
  describe "" $ do
    it "" $ do
      renderText (row_ $ columns_ "") `shouldBe` "<table class=\" row \"><tbody><tr><th class=\" columns \"><table><tr><th></th><th class=\"expander\"></th></tr></table></th></tr></tbody></table>"

      renderText (row_ $ do (with firstColumn_ [mkLarge 6] ""); (with firstColumn_ [mkLarge 6] "")) `shouldBe` "<table class=\" row \"><tbody><tr><th class=\" columns first large-6 small-12\"><table><tr><th></th><th class=\"expander\"></th></tr></table></th><th class=\" columns first large-6 small-12\"><table><tr><th></th><th class=\"expander\"></th></tr></table></th></tr></tbody></table>"

