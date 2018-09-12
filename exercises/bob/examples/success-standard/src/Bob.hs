module Bob (responseFor) where
import Data.Char (isSpace, isUpper, isAlpha, isPunctuation)
import Safe (lastMay)

data Prompt = Silence | YellQuestion | Yell | Question | Other

classify :: String -> Prompt
classify s | all isSpace s = Silence
           | yell && question = YellQuestion
           | yell = Yell
           | question = Question
           | otherwise = Other
           where yell = any isAlpha s && all isUpper (filter isAlpha s)
                 question = Just '?' == lastMay (filter isPunctuation s)

response :: Prompt -> String
response Silence = "Fine. Be that way!"
response YellQuestion = "Calm down, I know what I'm doing!"
response Yell = "Whoa, chill out!"
response Question = "Sure."
response Other = "Whatever."

responseFor :: String -> String
responseFor = response . classify
