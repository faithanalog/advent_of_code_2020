import Data.List (foldl', sort)

-- Back, Front, Left, Right (inclusive)
data Bound = Bound Int Int Int Int

cut :: Bound -> Char -> Bound
cut (Bound b f l r) 'F' = Bound b (div (b + f) 2) l r
cut (Bound b f l r) 'B' = Bound (1 + div (b + f) 2) f l r
cut (Bound b f l r) 'L' = Bound b f l (div (l + r) 2)
cut (Bound b f l r) 'R' = Bound b f (1 + div (l + r) 2) r

-- Bound ID, crashes if the bound does not refer to a specific seat
boundID :: Bound -> Int
boundID (Bound b f l r)
  | b == f && l == r =
    let row = b
        col = l
     in row * 8 + col

-- Apply a sequence of partitioning steps
findID :: String -> Int
findID s = boundID (foldl' cut (Bound 0 127 0 7) s)

-- Zip seat ID with the difference from the previous ID in the list
withDeltas :: [Int] -> [(Int, Int)]
withDeltas seats = zipWith (\prev next -> (next, next - prev)) seats (tail seats)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let seats = sort [findID ln | ln <- lines input]
      part1 = maximum seats
      part2 = head [seat - 1 | (seat, delta) <- withDeltas seats, delta == 2]

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)