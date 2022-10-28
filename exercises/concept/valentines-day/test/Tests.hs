import Test.Hspec (describe, hspec, it)
import ValentinesDay (Activity (..), Approval (..), Cuisine (..), Genre (..), rateActivity)

main :: IO ()
main = hspec $
  describe "ValentinesDay" $ do
    it "chill rated no" $
      case rateActivity Chill of
        No -> True
        _ -> False
    it "board game rated no" $
      case rateActivity BoardGame of
        No -> True
        _ -> False
    it "crime movie rated no" $
      case rateActivity (Movie Crime) of
        No -> True
        _ -> False
    it "horror movie rated no" $
      case rateActivity (Movie Horror) of
        No -> True
        _ -> False
    it "romance movie rated yes" $
      case rateActivity (Movie Romance) of
        Yes -> True
        _ -> False
    it "thriller movie rated no" $
      case rateActivity (Movie Thriller) of
        No -> True
        _ -> False
    it "korean restaurant rated yes" $
      case rateActivity (Restaurant Korean) of
        Yes -> True
        _ -> False
    it "turkish restaurant rated maybe" $
      case rateActivity (Restaurant Turkish) of
        Maybe -> True
        _ -> False
    it "walk 2 kilometers rated yes" $
      case rateActivity (Walk 2) of
        Yes -> True
        _ -> False
    it "walk 3 kilometers rated maybe" $
      case rateActivity (Walk 3) of
        Maybe -> True
        _ -> False
    it "walk 5 kilometers rated maybe" $
      case rateActivity (Walk 5) of
        Maybe -> True
        _ -> False
    it "walk 6 kilometers rated no" $
      case rateActivity (Walk 6) of
        No -> True
        _ -> False
