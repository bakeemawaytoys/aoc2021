data Command = Forward Nat | Down Nat | Up Nat

record Submarine where
    constructor SubmarineState
    position : Nat
    depth : Nat
    aim : Nat

initState : Submarine
initState = SubmarineState 0 0 0

processPart1Command : (x : Submarine) -> (y : Command) -> Submarine
processPart1Command x (Forward units) = record {position $= (+ units)} x
processPart1Command x (Down units) = record {depth $= (+ units)} x
processPart1Command x (Up units) = record {depth $= (\u => minus u units)} x

processPart2Command : (x : Submarine) -> (y : Command) -> Submarine
processPart2Command x (Forward units) = record {position $= (+ units), depth $= (\d => d + (mult units (aim x)))} x
processPart2Command x (Down units) = record {aim $= (+ units)} x
processPart2Command x (Up units) = record {aim $= (\u => minus u units)} x

processCommands : (Submarine -> Command -> Submarine ) -> Submarine -> List Command -> Submarine
processCommands commandProcessor submarine [] = submarine
processCommands commandProcessor submarine (command :: xs) = processCommands commandProcessor (commandProcessor submarine command) xs

finalResult : Submarine -> Nat
finalResult submarine = position submarine * depth submarine

isNumber : String -> Bool
isNumber str = all isDigit (unpack str)

parsingError : String -> Either String Command
parsingError invalidString = Left ("Could not parse '" ++ invalidString ++ "'")

parseLine : String -> Either String Command
parseLine x = case (Strings.split (== ' ') x) of
        "forward" :: unit :: Nil => handle Forward unit
        "down" :: unit :: Nil => handle Down unit
        "up" :: unit :: Nil => handle Up unit
        _ => parsingError x
    where
        handle : (Nat -> Command) -> String -> Either String Command
        handle command unit = if isNumber unit then Right (command (cast unit)) else parsingError x

parseLines : List String -> Either (List String)(List Command)
parseLines lines = 
    case Either.partitionEithers  (map parseLine lines) of
    ([], commands) => Right commands
    (errors, _) => Left errors

main: IO()
main = do
        file <- readFile "input.txt"
        case file of 
            Right content => 
                case parseLines (lines content) of
                    Right commands => 
                        let
                            part1Solution = finalResult (processCommands processPart1Command initState commands)
                            part2Solution = finalResult (processCommands processPart2Command initState commands)
                            output = ("Part 1: " ++ (show part1Solution) ++ "\nPart2: " ++ (show part2Solution))
                        in
                            putStrLn output
                    Left errors => putStrLn (unlines errors)
            Left err => printLn err


testData : List Command
testData = [
            Forward 5,
            Down 5,
            Forward 8,
            Up 3,
            Down 8,
            Forward 2
        ]

testPart1 : Nat
testPart1 =  finalResult (processCommands processPart1Command initState testData)

testPart2 : Nat
testPart2 = finalResult (processCommands processPart2Command initState testData)