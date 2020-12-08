{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (Foldable (fold, toList))
import Data.Monoid (First (First))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- Generating and using instructions
type Instr = (Text, Int)

type Program = Seq Instr

runInstr :: Instr -> Int -> (Int, Int)
runInstr ("jmp", offset) accumulator = (offset, accumulator)
runInstr ("acc", delta) accumulator = (1, accumulator + delta)
runInstr ("nop", _) accumulator = (1, accumulator)

parseInstr :: Text -> Instr
parseInstr ln =
  let [name, argText] = Text.splitOn " " ln
      argVal = read (filter (/= '+') (Text.unpack argText))
   in (name, argVal)

parseProg :: Text -> Program
parseProg input = Seq.fromList (map parseInstr (Text.lines input))

-- Tape which tracks whether an instruction has been executed
type Tape = Seq (Instr, Bool)

-- Execute until out-of-bounds or an instruction is executed twice.
-- Return final program counter and accumulator
execTape :: Int -> Int -> Tape -> (Int, Int)
execTape idx acc prog =
  case Seq.lookup idx prog of
    Nothing -> (idx, acc)
    Just (_, True) -> (idx, acc)
    Just (instr, False) ->
      let (offset, newAcc) = runInstr instr acc
          newProgram = Seq.update idx (instr, True) prog
       in execTape (idx + offset) newAcc newProgram

execProgram :: Program -> (Int, Int)
execProgram prog = execTape 0 0 (Seq.fromList (zip (toList prog) (repeat False)))

-- Repair such that the program terminates by executing the index after the last instruction
repairAndExec :: Program -> Int
repairAndExec prog =
  let swap ("jmp", x) = ("nop", x)
      swap ("nop", x) = ("jmp", x)
      swap other = other
      swapIndex idx = Seq.adjust swap idx prog
      tryRepair idx =
        case execProgram (swapIndex idx) of
          (idx, acc) | idx == Seq.length prog -> Just acc
          _ -> Nothing
      First (Just result) = fold [First (tryRepair idx) | idx <- [0 .. Seq.length prog - 1]]
   in result

main :: IO ()
main = do
  input <- Text.readFile "input.txt"
  let prog = parseProg input
      (_, part1) = execProgram prog
      part2 = repairAndExec prog

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)