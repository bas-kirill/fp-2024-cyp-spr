module Lib
  ( AgeOnPlanetErr (UnknownPlanet, PlutoNotPlanet),
    LeapYearErr (NegativeYear),
    ageOn,
    isLeapYear,
  )
where

-- 1. Given an age in seconds, calculate how old (in years) someone would be on a different planet.
-- Implement a function `ageOn planet ageInSeconds` which returns the age in years represented as a floating point number of type `Float`.
-- Planet name is a `String`, case sensitive. An error should be reported if an unrecognized planet is passed into.
-- If someone tried to find their age on Pluto, explain to them that it's not a planet.
--    * Mercury: orbital period 0.2408467 Earth years
--    * Venus: orbital period 0.61519726 Earth years
--    * Earth: orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
--    * Mars: orbital period 1.8808158 Earth years
--    * Jupiter: orbital period 11.862615 Earth years
--    * Saturn: orbital period 29.447498 Earth years
--    * Uranus: orbital period 84.016846 Earth years
--    * Neptune: orbital period 164.79132 Earth years

data AgeOnPlanetErr = UnknownPlanet | PlutoNotPlanet deriving (Show, Eq)

type AgeResult = Either AgeOnPlanetErr Float

ageOn :: Integer -> String -> AgeResult
ageOn ageInSeconds planet =
  case planet of
    "Mercury" -> Right (fromInteger ageInSeconds / 31557600 / 0.2408467)
    "Venus" -> Right (fromInteger ageInSeconds / 31557600 / 0.61519726)
    "Earth" -> Right (fromInteger ageInSeconds / 31557600 / 1.0)
    "Mars" -> Right (fromInteger ageInSeconds / 31557600 / 1.8808158)
    "Jupiter" -> Right (fromInteger ageInSeconds / 31557600 / 11.862615)
    "Saturn" -> Right (fromInteger ageInSeconds / 31557600 / 29.447498)
    "Uranus" -> Right (fromInteger ageInSeconds / 31557600 / 84.016846)
    "Neptune" -> Right (fromInteger ageInSeconds / 31557600 / 164.79132)
    "Pluto" -> Left PlutoNotPlanet
    _ -> Left UnknownPlanet

-- 2. Given a year, report if it is a leap year (function `isLeapYear year`). A leap year in the Gregorian calendar occurs:
-- - on every year that is evenly divisible by 4
-- - except every year that is evenly divisible by 100
-- - unless the year is also evenly divisible by 400
-- For example, 1997 is not a leap year, but 1996 is. 1900 is not a leap year, but 2000 is. Report an error, if the year is negative.

data LeapYearErr = NegativeYear

type IsLeapYearResult = Either LeapYearErr Bool

isLeapYear :: Integer -> IsLeapYearResult
isLeapYear year
  | year < 0 = Left NegativeYear
  | year `mod` 400 == 0 = Right True
  | year `mod` 100 == 0 = Right False
  | year `mod` 4 == 0 = Right True
  | otherwise = Right False
