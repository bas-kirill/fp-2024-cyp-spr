import Lib
  ( AgeOnPlanetErr (PlutoNotPlanet, UnknownPlanet),
    LeapYearErr (NegativeYear),
    ageOn,
    isLeapYear,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

testAgeOn :: TestTree
testAgeOn =
  testGroup "AgeOn" [testAgeOn]
  where
    testAgeOn =
      testGroup
        "Add"
        [ testCase "age on Earth -- real time" $ do
            let realTime = 31557600 `ageOn` "Earth"
            case realTime of
              Right age -> age @?= 1.0
              Left err -> assertFailure $ show err,
          testCase "unknown planet -- error" $ do
            let realTime = 100 `ageOn` "404"
            case realTime of
              Right age -> assertFailure $ show age
              Left err -> err @?= UnknownPlanet,
          testCase "passed pluto -- error" $ do
            let realTime = 100500 `ageOn` "Pluto"
            case realTime of
              Right age -> assertFailure $ show age
              Left err -> err @?= PlutoNotPlanet
        ]

testIsLeapYear :: TestTree
testIsLeapYear =
  testGroup "IsLeapYear" [testIsLeapYear]
  where
    testIsLeapYear =
      testGroup
        "IsLeapYear"
        [ testCase "`2004` year -- leap year" $ do
            let leapYear = isLeapYear 2004
            case leapYear of
              Right isLeap -> isLeap @?= True
              Left _ -> assertFailure "Expected a boolean result",
          testCase "`400` year -- leap year" $ do
            let leapYear = isLeapYear 2000
            case leapYear of
              Right isLeap -> isLeap @?= True
              Left _ -> assertFailure "Expected a boolean result",
          testCase "`1900` year -- not leap year" $ do
            let leapYear = isLeapYear 1900
            case leapYear of
              Right isLeap -> isLeap @?= False
              Left _ -> assertFailure "Expected a boolean result",
          testCase "`2005` year -- not leap year" $ do
            let leapYear = isLeapYear 2005
            case leapYear of
              Right isLeap -> isLeap @?= False
              Left _ -> assertFailure "Expected a boolean result",
          testCase "negative year -- error" $ do
            let leapYear = isLeapYear (-100)
            case leapYear of
              Right _ -> assertFailure "Expected an error"
              Left NegativeYear -> assertBool "Negative year error" True
        ]

main :: IO ()
main = defaultMain $ testGroup "HW01" [testAgeOn, testIsLeapYear]
