
import Lucid.Base
import Stinky
import Test.Hspec

main :: IO ()
main = hspec $
  describe "" $ do
    it "" $ do
      renderText (row_ (columns_ "")) `shouldBe` "<table class=\" row \"><tbody><tr><th class=\" columns \"><table><tr><th></th><th class=\"expander\"></th></tr></table></th></tr></tbody></table>"
