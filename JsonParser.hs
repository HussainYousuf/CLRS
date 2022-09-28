import Data.Char
import Control.Applicative


data JsonValue = JsonNull
    | JsonNumber Integer
    | JsonBool Bool
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving Show

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
}

instance Functor Parser where 
    fmap f (Parser p) = Parser $ \input -> do
        (input', a) <- p input
        return (input', f a)

instance Applicative Parser where
    pure a = Parser $ \input -> Just (input, a)
    Parser p1 <*> Parser p2 = Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        return (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser p1 <|> Parser p2 = Parser $ \input -> 
        p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = Parser $ \input -> 
    case input of
        (x:xs) | c == x -> Just (xs, x) 
        _ -> Nothing

stringP :: String -> Parser String
stringP s = sequenceA $ map charP s

jsonNull :: Parser JsonValue
jsonNull = const JsonNull <$> stringP "null"

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, input') = span f input in 
    Just (input', token)

notNullP :: Parser String -> Parser String
notNullP (Parser p) = Parser $ \input -> do
    res@(input', token) <- p input
    if (null token) then Nothing
    else return res

jsonNumber :: Parser JsonValue
jsonNumber = (JsonNumber . read) <$> (notNullP $ spanP isDigit)

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false") 
    where 
        f "true" = JsonBool True
        f "false" = JsonBool False

stringLiteralP :: Parser String
stringLiteralP = spanP (/= '"') 

whiteSpaceP :: Parser String
whiteSpaceP = spanP isSpace 

quotedString :: Parser String
quotedString = charP '"' *> stringLiteralP <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> quotedString

commaSepP :: Parser Char
commaSepP = whiteSpaceP *> charP ',' <* whiteSpaceP

colonSepP :: Parser Char
colonSepP = whiteSpaceP *> charP ':' <* whiteSpaceP
-- elements :: Parser JsonValue
-- elements = (sep *> jsonValue) <|> (jsonValue <* sep) <|> jsonValue

sepByP :: Parser Char -> Parser a -> Parser [a]
sepByP sep element = (liftA2 (:) element (many $ sep *> element)) <|> pure []  

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' 
            *> whiteSpaceP *> 
            (sepByP commaSepP jsonValue)
             <* whiteSpaceP <*
            charP ']') 

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' 
            *> whiteSpaceP *> 
            (sepByP commaSepP pairs)
            <* whiteSpaceP <* 
            charP '}')
    where 
        pairs = (\key _ value -> (key, value)) <$> quotedString <*> colonSepP <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonNumber <|> jsonBool <|> jsonString <|> jsonArray <|> jsonObject

main :: IO()
main = do 
    json <- readFile "./dummyJson"
    print $ snd <$> runParser jsonValue json