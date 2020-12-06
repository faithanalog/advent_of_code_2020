{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  input <- Text.readFile "input.txt"
  let groups = [Text.unpack group | group <- Text.splitOn "\n\n" input]
      part1 = sum [length (nub (sort (filter isAlpha group))) | group <- groups]
      part2 = sum [length (foldr intersect ['a'..'z'] (lines group)) | group <- groups]

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)