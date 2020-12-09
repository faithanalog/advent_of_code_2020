import Data.Maybe (mapMaybe)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

type CypherText = Vector Int

parse :: String -> CypherText
parse str = Vector.fromList (map read (lines str))

valid :: CypherText -> Int -> Bool
valid txt idx =
  let pre = Vector.slice (idx - 25) 25 txt
      val = txt ! idx
      match a b = a + b == val
   in Vector.any (\a -> Vector.any (match a) pre) pre

attackFrom :: CypherText -> Int -> Int -> Maybe Int
attackFrom txt target idx =
  let weakness len =
        let slice = Vector.slice idx len txt
         in Vector.minimum slice + Vector.maximum slice
      slices = takeWhile (<= target) (scanl1 (+) (Vector.toList (Vector.drop idx txt)))
   in case last (zip [1 ..] slices) of
        (len, acc) | acc == target -> Just (weakness len)
        _ -> Nothing

attack :: CypherText -> Int -> Int
attack txt target =
  head (mapMaybe (attackFrom txt target) [0 ..])

main :: IO ()
main = do
  input <- readFile "input.txt"
  let cypherText = parse input
      part1 = cypherText ! head (filter (not . valid cypherText) [25 ..])
      part2 = attack cypherText part1

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)