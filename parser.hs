import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

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
--    c <- oneOf ['\\','"','n','r','t']
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
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do
--     num <- many1 digit
--     return $ Number . read $ num
parseNumber = (many1 digit) >>= (\x -> return ((Number . read) x))

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
