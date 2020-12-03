-- required packages: vector
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

data World = World
  { worldPeriod :: Int,
    worldHeight :: Int,
    worldData :: Vector Bool
  }

worldFromString :: String -> World
worldFromString str =
  let lns = lines str
   in World
        { worldPeriod = length (head lns),
          worldHeight = length lns,
          worldData = Vector.fromList [tile == '#' | tile <- concat lns]
        }

tileAt :: World -> Int -> Int -> Bool
tileAt world x y =
  let wrappedX = mod x (worldPeriod world)
      idx = wrappedX + (y * worldPeriod world)
   in worldData world ! idx

treeEncounters :: World -> Int -> Int -> Int
treeEncounters world dx dy =
  let coords = zip [0, dx ..] [0, dy .. worldHeight world - 1]
      collisions = [() | (x, y) <- coords, tileAt world x y]
   in length collisions

main :: IO ()
main = do
  input <- readFile "input.txt"
  let world = worldFromString input
      part1 = treeEncounters world 3 1
      part2 =
        product
          [ treeEncounters world 1 1,
            treeEncounters world 3 1,
            treeEncounters world 5 1,
            treeEncounters world 7 1,
            treeEncounters world 1 2
          ]

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)