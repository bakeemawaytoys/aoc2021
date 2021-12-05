import Data.String

total depthIncreaseCount : List Int -> Nat
depthIncreaseCount [] = 0
depthIncreaseCount (x :: xs) = countChanges 0 x xs
    where
        countChanges : (counter : Nat) -> (previous : Int) -> (depths : List Int) -> Nat
        countChanges counter previous [] = counter
        countChanges counter previous (x :: xs) = 
            if 
                x > previous 
            then 
                countChanges (counter + 1)  x xs 
            else countChanges counter x xs

testPart1 : Nat
testPart1 = depthIncreaseCount [ 199, 200, 208, 210, 200,  207, 240, 269, 260, 263 ]

windowedList : List Int -> List Int
windowedList [] = []
windowedList (x :: []) = []
windowedList (x :: y :: []) = []
windowedList (x :: y :: z :: xs) = (x + y + z) :: (windowedList (y :: z :: xs))

testPart2 : Nat
testPart2 = depthIncreaseCount (windowedList [ 199, 200, 208, 210, 200,  207, 240, 269, 260, 263 ])

parseFileContent : String -> List Int
parseFileContent content = 
     let 
          lines = Strings.lines content
          parsed = map Data.String.parseInteger lines
     in
          List.mapMaybe id parsed

main : IO ()
main = do
        file <- readFile "input.txt"
        case file of
               Right content => let 
                    depths = parseFileContent content
                    part1Solution = depthIncreaseCount depths
                    part2Solution =  depthIncreaseCount (windowedList depths)
                    output = "Part 1: " ++ (show part1Solution) ++ "\nPart 2: " ++ (show part2Solution)
                in
                    putStrLn  (output)
               Left err => printLn err

