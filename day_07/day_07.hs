{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative ((<|>)), optional)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- Map Bag to Parents
type BagParents = Map Text [Text]

-- Map Bag to Children
type BagChildren = Map Text [(Text, Int)]

-- arrow reversal here
-- Parent -> [Child] becomes Child -> [Parent]
insertParents :: Text -> [Text] -> BagParents -> BagParents
insertParents outer inner =
  let insertOne bag =
        Map.alter
          ( \current -> case current of
              Nothing -> Just [outer]
              Just outers -> Just (outer : outers)
          )
          bag
   in foldr (.) id (map insertOne inner)

name :: Parser Text
name = do
  left <- Atto.takeWhile1 (/= ' ')
  " "
  right <- Atto.takeWhile1 (/= ' ')
  " bag"
  optional "s"
  return (Text.concat [left, " ", right])

parseInner :: Parser (Text, Int)
parseInner = do
  count <- Atto.decimal
  " "
  bag <- name
  return (bag, count)

parseOneBag :: Parser (Text, [(Text, Int)])
parseOneBag = do
  outer <- name
  " contain "
  inners <- Atto.sepBy1 parseInner ", " <|> ("no other bags" *> return [])
  "."
  return (outer, inners)

parseBags :: Parser (BagParents, BagChildren)
parseBags = do
  inToOutMappings <- Atto.sepBy1 parseOneBag "\n"
  let children = Map.fromList inToOutMappings
      parents = foldr1 (.) [insertParents outer (map fst inners) | (outer, inners) <- inToOutMappings] Map.empty
  return (parents, children)

parentsOf :: BagParents -> Text -> Set Text
parentsOf bags target =
  let recurse nextSource
        | nextSource == target = Set.singleton nextSource
        | otherwise = Set.singleton nextSource <> foldMap recurse (Map.findWithDefault [] nextSource bags)
   in foldMap recurse (Map.findWithDefault [] target bags)

countChildrenOf :: BagChildren -> Text -> Int
countChildrenOf bags target =
  let children = Map.findWithDefault [] target bags
   in sum [n + n * countChildrenOf bags bag | (bag, n) <- children]

main :: IO ()
main = do
  input <- Text.readFile "input.txt"
  let Right (parents, children) = Atto.parseOnly parseBags input
      part1 = length (parentsOf parents "shiny gold")
      part2 = countChildrenOf children "shiny gold"

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)
