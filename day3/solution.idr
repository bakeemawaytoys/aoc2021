import Data.Vect

data Bit = One | Zero

Show Bit where
    show One = "1"
    show Zero = "0"

negate : Bit -> Bit
negate One = Zero
negate Zero = One

flip : Vect n Bit -> Vect n Bit
flip v = map negate v

showBinary: Vect n Bit -> String
showBinary xs = foldl (Strings.(++)) "" (map show xs) 

isBit: Char -> Bool
isBit '0' = True
isBit '1' = True
isBit  _  = False

binaryToDecimal: Vect n Bit -> Nat
binaryToDecimal  v = snd (foldr handle (0,0) (toList v))
    where
        handle : (bit: Bit) -> (Nat,Nat) -> (Nat, Nat)
        handle One (position, acc) = (position + 1, (power 2 position) + acc)
        handle Zero (position, acc) =(position + 1, acc)

gamma: Vect n (Vect m Bit) -> Vect m Bit
gamma vec =
    let
        transposed = Vect.transpose vec
        digits = map (\bit => map bitToDigit bit) transposed
        summed = map (\vec => sum vec) digits
    in
        map (\count => if count > 0 then One else Zero) summed
    where
        bitToDigit: Bit -> Int
        bitToDigit One = 1
        bitToDigit Zero = -1

epsilon: Vect n (Vect m Bit) -> Vect m Bit
epsilon vec = flip (gamma vec)

powerConsumption: Vect n (Vect m Bit) -> Nat
powerConsumption vect = 
    let
        g = gamma vect
        e = epsilon vect
    in
        (binaryToDecimal g) * (binaryToDecimal e)

charListToVect: (n : Nat) -> List Char -> Either String (Vect n Char)
charListToVect Z [] = Right []
charListToVect Z (x :: xs) = Left ""
charListToVect (S k) [] =  Left ""
charListToVect (S k) (x :: xs) = 
    do
        v <- charListToVect k xs
        Right (x :: v)

{- charListToVect lst = Left ("The string '" ++ (Strings.pack lst) ++ "' is not exactly five characters.") -}

charsToBits: Vect n Char -> Either String (Vect n Bit)
charsToBits str = 
    if 
        all isBit str 
    then 
        Right (map convert str) 
    else 
        Left ("The string '" ++ (show str) ++ "' is not a bit string.")
    where
        convert: Char -> Bit
        convert '0' = Zero
        convert _ = One

total parseLine: (n : Nat) -> String -> Either String (Vect n Bit)
parseLine n str = 
    do
        chars <- charListToVect n (Strings.unpack str)
        charsToBits chars

total lToV :  (l: List elem ) -> (n ** Vect n elem)
lToV [] = (_ ** [])
lToV (x :: xs ) = let
                    rst = lToV xs
                    lst = snd rst
                in
                    (_ ** (x :: lst))

total parseLines: (n : Nat) -> String -> Either String (m ** Vect m (Vect n Bit))
parseLines n str = 
                let
                    pl = map (parseLine n) (Strings.lines str)
                    partitioned = partitionEithers pl
                in
                    case partitioned of
                        ([], l) => Right (lToV l)
                        (errors, _) => Left (Strings.unlines errors)

testData: List String
testData = [
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
]

testPart1 : String
testPart1 = case parseLines 5 (Strings.unlines testData) of
    Right (len ** words) => show (powerConsumption words)
    Left msg => msg


main : IO ()
main = do
            Right input <- readFile "input.txt" | Left msg => printLn msg
            Right (len ** inputLines) <- pure (parseLines 12 input) | Left msg => putStrLn msg
            printLn (powerConsumption inputLines)