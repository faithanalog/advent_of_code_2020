-- required packages: containers, text, attoparsec
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Passport = Map Text Text

data FieldValidator = FieldValidator Text (Text -> Bool)

parseField :: Text -> (Text, Text)
parseField field =
  let (key, value) = Text.span (/= ':') field
   in (key, Text.tail value)

parsePassport :: Text -> Passport
parsePassport s = Map.fromList [parseField field | field <- Text.words s]

parseAllPassports :: Text -> [Passport]
parseAllPassports s = [parsePassport passportText | passportText <- Text.splitOn "\n\n" s]

mkValidator :: Text -> Parser Bool -> FieldValidator
mkValidator key parser =
  let validate s =
        case Atto.parseOnly (parser <* Atto.endOfInput) s of
          Left _ -> False
          Right valid -> valid
   in FieldValidator key validate

vaildateBYR :: FieldValidator
vaildateBYR = mkValidator "byr" $ do
  year <- Atto.decimal
  return (year >= 1920 && year <= 2002)

validateIYR :: FieldValidator
validateIYR = mkValidator "iyr" $ do
  year <- Atto.decimal
  return (year >= 2010 && year <= 2020)

validateEYR :: FieldValidator
validateEYR = mkValidator "eyr" $ do
  year <- Atto.decimal
  return (year >= 2020 && year <= 2030)

validateHGT :: FieldValidator
validateHGT = mkValidator "hgt" $ do
  height <- Atto.decimal
  units <- "cm" <|> "in"
  case units of
    "cm" -> return (height >= 150 && height <= 193)
    "in" -> return (height >= 59 && height <= 76)

validateHCL :: FieldValidator
validateHCL = mkValidator "hcl" $ do
  _ <- Atto.char '#'
  color <- Atto.takeWhile1 (Atto.inClass "0-9a-f")
  return (Text.length color == 6)

validateECL :: FieldValidator
validateECL = mkValidator "ecl" $ do
  _ <-
    "amb"
      <|> "blu"
      <|> "brn"
      <|> "gry"
      <|> "grn"
      <|> "hzl"
      <|> "oth"
  return True

validatePID :: FieldValidator
validatePID = mkValidator "pid" $ do
  pid <- Atto.takeWhile1 isDigit
  return (Text.length pid == 9)

allValidators :: [FieldValidator]
allValidators =
  [ vaildateBYR,
    validateIYR,
    validateEYR,
    validateHGT,
    validateHCL,
    validateECL,
    validatePID
  ]

runValidator :: Passport -> FieldValidator -> Bool
runValidator passport (FieldValidator key check) =
  case Map.lookup key passport of
    Nothing -> False
    Just val -> check val

hasRequiredFields :: Passport -> Bool
hasRequiredFields passport =
  let requiredFields = Set.fromList [key | FieldValidator key _ <- allValidators]
   in Set.isSubsetOf requiredFields (Map.keysSet passport)

hasValidFields :: Passport -> Bool
hasValidFields passport = all (runValidator passport) allValidators

main :: IO ()
main = do
  input <- Text.readFile "input.txt"
  let passports = parseAllPassports input
      part1 = length (filter hasRequiredFields passports)
      part2 = length (filter hasValidFields passports)

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)