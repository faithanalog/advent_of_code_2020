-- I was gonna do this a weird way but i can't think of anything interesting

main :: IO ()
main = do
  input <- readFile "input.txt"
  let nums = [read i | i <- lines input]
      part1 =
        head
          [ x * y
            | x <- nums,
              y <- nums,
              x + y == 2020
          ]
      part2 =
        head
          [ x * y * z
            | x <- nums,
              y <- nums,
              z <- nums,
              x + y + z == 2020
          ]
  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)