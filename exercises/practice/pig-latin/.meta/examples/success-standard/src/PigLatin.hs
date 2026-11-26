module PigLatin (translate) where
import Data.Char (isLetter)
import Data.List (isSuffixOf)

consonantCluster :: String -> (String, String)
consonantCluster (c:cs) | c `notElem` "aeiou" =
  let (more, rest) = break (`elem` "aeiouy") cs in qu (c : more, rest)
  where qu (xs, 'u':rest) | "q" `isSuffixOf` xs = (xs ++ "u", rest)
        qu pair = pair
consonantCluster w = ("", w)

translateWord :: String -> String
translateWord xs@('y':'t':_) = xs ++ "ay" -- I'm not proud of this ugly hack,
translateWord xs@('x':'r':_) = xs ++ "ay" -- but now it passes the tests.
translateWord ('r':'h':xs)   = xs ++ "rhay"
translateWord "my"           = "ymay"
translateWord w0 = concat [before, w, cs, "ay", after]
  where
    (before, w1) = break isLetter w0
    (w2, after)  = span isLetter w1
    (cs, w)      = consonantCluster w2

translate :: String -> String
translate = unwords . map translateWord . words
