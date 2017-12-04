module Bob (responseFor) where
import Data.Char (isSpace, isUpper, isAlpha)

data Prompt = Silence | YellQuestion | Yell | Question | Other

classify :: String -> Prompt
classify s | all isSpace s = Silence
           | yell && question = YellQuestion
           | yell = Yell
           | question = Question
           | otherwise = Other
           where yell = any isAlpha s && all isUpper (filter isAlpha s)
                 question = '?' == last (filter (not . isSpace) s)

response :: Prompt -> String
response Silence = "Fine. Be that way!"
response YellQuestion = "Calm down, I know what I'm doing!"
response Yell = "Whoa, chill out!"
response Question = "Sure."
response Other = "Whatever."

responseFor :: String -> String
responseFor = response . classify
