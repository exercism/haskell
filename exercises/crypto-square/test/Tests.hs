import Test.HUnit ((@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import CryptoSquare (normalizePlaintext,
                     squareSize,
                     plaintextSegments,
                     ciphertext,
                     normalizeCiphertext)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestLabel "normalizePlaintext" $ TestList normalizePlaintextTests
       , TestLabel "squareSize" $ TestList squareSizeTests
       , TestLabel "plaintextSegments" $ TestList plaintextSegmentsTests
       , TestLabel "ciphertext" $ TestList ciphertextTests
       , TestLabel "normalizeCiphertext" $ TestList normalizeCiphertextTests
       ]

normalizePlaintextTests :: [Test]
normalizePlaintextTests = map TestCase
  [ "splunk" @=? normalizePlaintext "s#!@$%plunk"
  , "123go" @=? normalizePlaintext "1, 2, 3 GO!"
  ]

squareSizeTests :: [Test]
squareSizeTests = map TestCase
  [ 2 @=? squareSize "1234"
  , 3 @=? squareSize "123456789"
  , 4 @=? squareSize "123456789abc" ]

plaintextSegmentsTests :: [Test]
plaintextSegmentsTests = map TestCase
  [ ["neverv", "exthin", "eheart", "withid", "lewoes"] @=?
    plaintextSegments "Never vex thine heart with idle woes."
  , ["zomg", "zomb", "ies"] @=?
    plaintextSegments "ZOMG! ZOMBIES!!!"
  ]

ciphertextTests :: [Test]
ciphertextTests = map TestCase
  [ "tasneyinicdsmiohooelntuillibsuuml" @=?
    ciphertext "Time is an illusion. Lunchtime doubly so."
  , "wneiaweoreneawssciliprerlneoidktcms" @=?
    ciphertext "We all know interspecies romance is weird."
  ]

normalizeCiphertextTests :: [Test]
normalizeCiphertextTests = map TestCase
  [ "msemo aanin dnin ndla etlt shui" @=?
    normalizeCiphertext "Madness, and then illumination."
  , "vrel aepe mset paoo irpo" @=?
     normalizeCiphertext "Vampires are people too!"
  , "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn sseoau" @=?
     normalizeCiphertext "If man was meant to stay on the ground god would \
                         \have given us roots"
  ]
