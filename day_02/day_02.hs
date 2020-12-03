import Data.Char (isDigit)

data Pass = Pass Int Int Char String

-- password parser state machine
data ParseState
  = LowBound
  | HighBound Int
  | ReqChar Int Int
  | UserPass Int Int Char

-- password parser impl
parsePass :: ParseState -> String -> Pass
-- consume ex: `14`
parsePass LowBound s =
  let (field, rem) = span isDigit s
   in parsePass (HighBound (read field)) rem
-- consume ex: `-19`
parsePass (HighBound low) ('-' : s) =
  let (field, rem) = span isDigit s
   in parsePass (ReqChar low (read field)) rem
-- consume ex: ` m`
parsePass (ReqChar low high) (' ' : reqChar : rem) =
  parsePass (UserPass low high reqChar) rem
-- consume ex: `: lvgmmmwmmmmmmmlmmmz`
parsePass (UserPass low high reqChar) (':' : ' ' : pw) =
  Pass low high reqChar pw

validPart1 :: Pass -> Bool
validPart1 (Pass low high reqChar pw) =
  let occurrences = length (filter (== reqChar) pw)
   in occurrences >= low && occurrences <= high

validPart2 :: Pass -> Bool
validPart2 (Pass low high reqChar pw) =
  let char1 = (pw !! (low - 1))
      char2 = (pw !! (high - 1))
   in (char1 == reqChar || char2 == reqChar) && char1 /= char2

main :: IO ()
main = do
  input <- readFile "input.txt"
  let passwords = [parsePass LowBound ln | ln <- lines input]
      part1 = length (filter validPart1 passwords)
      part2 = length (filter validPart2 passwords)

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)