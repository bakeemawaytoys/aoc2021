data Command = Forward Nat | Down Nat | Up Nat

record Submarine where
    constructor SubmarineState
    position : Nat
    depth : Nat

initState : Submarine
initState = SubmarineState 0 0

processCommand : (x : Submarine) -> (y : Command) -> Submarine
processCommand x (Forward units) = record {position $= (+ units)} x
processCommand x (Down units) = record {depth $= (+ units)} x
processCommand x (Up units) = record {depth $= (\u => minus u units)} x

processCommands : Submarine -> List Command -> Submarine
processCommands x [] = x
processCommands x (y :: xs) = processCommands (processCommand x y) xs

finalResult : Submarine -> Nat
finalResult (SubmarineState position depth) = position * depth

testPart1 : Nat
testPart1 = 
    let 
        commands = [
            Forward 5,
            Down 5,
            Forward 8,
            Up 3,
            Down 8,
            Forward 2
        ]
    in
        finalResult (processCommands initState commands)

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
                            part1Solution = finalResult (processCommands initState commands)
                            output = ("Part 1: " ++ (show part1Solution))
                        in
                            putStrLn output
                    Left errors => putStrLn (unlines errors)
            Left err => printLn err