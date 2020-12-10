import Data.IntMap ((!))
import qualified Data.IntMap.Lazy as IntMap
import Data.List (sort, tails)

-- takes a sorted descending list
calc :: [Int] -> Int
calc xs = cc ! head xs
  where
    cc =
      IntMap.fromList
        [(head xtail, count xtail) | xtail <- init (tails xs)]
    count [0] = 1
    count (a : xs) = sum [cc ! b | b <- take 3 xs, a - b <= 3]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let vals = sort (map read (lines input))
      jolts = 0 : vals ++ [last vals + 3]
      diffs = zipWith subtract jolts (tail jolts)
      ones = length (filter (== 1) diffs)
      threes = length (filter (== 3) diffs)
      part1 = ones * threes
      part2 = calc (reverse jolts)

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)