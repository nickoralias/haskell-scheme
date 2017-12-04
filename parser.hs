import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char

parseChar :: Parser LispVal
parseChar = do
    string "#\\"
    s <- many1 letter
    return $ case s of
        "space" -> Character ' '
        "newline" -> Character '\n'
        [c] -> Character c

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)    

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"" <|> escapedChars)
    char '"'
    return $ String x

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    c <- oneOf("\"nrt\\")
    return $ case c of
        '\\' -> c
        '"' -> c
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseRadixNumber

parsePlainNumber :: Parser LispVal
parsePlainNumber = many1 digit >>= return . Number . read

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >>
                    ((char 'd' >> parsePlainNumber)
                     <|> (char 'b' >> parseBinary)
                     <|> (char 'o' >> parseOctal)
                     <|> (char 'x' >> parseHex))

parseBinary = parseNumberInBase "01" 2
parseOctal = parseNumberInBase "01234567" 8
parseHex = parseNumberInBase "0123456789abcdefABCDEF" 16

parseNumberInBase :: String -> Integer -> Parser LispVal
parseNumberInBase digits base = do
    d <- many (oneOf (digits))
    return $ Number $ toDecimal base d

toDecimal :: Integer -> String -> Integer
toDecimal base s = foldl1 ((+) . (* base)) $ map toNumber s
                    where toNumber = (toInteger . digitToInt)

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
