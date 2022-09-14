module ValentinesDay (rateActivity, Approval (..), Cuisine (..), Genre (..), Activity (..)) where

data Approval
  = Yes
  | No
  | Maybe

data Cuisine
  = Korean
  | Turkish

data Genre
  = Crime
  | Horror
  | Romance
  | Thriller

data Activity
  = BoardGame
  | Chill
  | Movie Genre
  | Restaurant Cuisine
  | Walk Int

rateActivity :: Activity -> Approval
rateActivity activity =
  case activity of
    Restaurant Korean -> Yes
    Restaurant Turkish -> Maybe
    Movie Romance -> Yes
    Movie _ -> No
    Walk kilometers
      | kilometers < 3 -> Yes
      | kilometers <= 5 -> Maybe
    _ -> No
