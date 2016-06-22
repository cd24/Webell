module Parser where
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Webell

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
   fmap f v = Parser g
     where
       g [] = Nothing
       g xs = case runParser v xs of
               Just l -> Just (first f l)
               Nothing -> Nothing

instance Applicative Parser where
  pure l = Parser (\x -> Just (l, x))
  p1 <*> p2 = Parser f
      where
        f xs = case runParser p1 xs of
          Nothing -> Nothing
          Just (l, rest) -> case runParser p2 rest of
            Nothing -> Nothing
            Just (a1, end) -> Just (l a1, end)

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser f
    where
      f xs = case runParser p1 xs of
        Just l -> Just l
        Nothing -> runParser p2 xs

parseDomElement :: Parser (Tag a)
parseDomElement = parseHask <|> parseSelfClosing <|> parseTag <|> parseValue

parseDomElements :: Parser [Tag a]
parseDomElements = many parseDomElement

ignoreWhitespace :: Parser a -> Parser a
ignoreWhitespace after = many ((char ' ' <|> char '\t') <|> char '\n') *> after

parseString :: String -> Parser String
parseString phrase = sequenceA (satisfy . (==) <$> phrase)

parseOpen :: Parser String
parseOpen = parseString "<hask>"

parseClose :: Parser String
parseClose = parseString "</hask>"

parseTagOpen :: Parser Char
parseTagOpen = char '<'

parseTagClose :: Parser Char
parseTagClose = char '>'

parseBody :: Parser String
parseBody = Parser f where
    f xs | null ns = Nothing
         | otherwise = Just (ns, remaining)
         where (ns, remaining) = break isTag xs


parseLoneChar :: Char -> Parser TagOption
parseLoneChar end = Parser f where
    f xs  | null op = Nothing
          | otherwise = Just (Lone op, remaining)
          where (op, remaining) = readExcluding end ['='] ("", xs)

parseLoneSpaced :: Parser TagOption
parseLoneSpaced = parseLoneChar ' '

parseLoneClosed :: Parser TagOption
parseLoneClosed = parseLoneChar '>'

parseLone :: Parser TagOption
parseLone = parseLoneSpaced <|> parseLoneClosed

parseTO :: Parser TagOption
parseTO = (\left _ right -> TO left right) <$> readUntil '=' <*> char '=' <*> (readUntil ' ' <|> readUntil '>')

parseTagOption :: Parser TagOption
parseTagOption = ignoreWhitespace (parseTO <|> parseLone)

parseTagOptions :: Parser [TagOption]
parseTagOptions = many parseTagOption <|> pure []

parseHask :: (HTML a) => Parser (Tag a)
parseHask = Hask <$> (parseOpen *>
                     readUntil '<' <*
                     parseClose)

parseTag :: HTML a => Parser (Tag a)
parseTag = Tag <$> (parseTagOpen *> ignoreWhitespace (readUntil ' ')) <*> (parseTagOptions <* parseTagClose) <*> parseDomElements

parseValue :: Parser (Tag String)
parseValue = Value <$> readUntil '<'

parseSelfClosing :: HTML a => Parser (Tag a)
parseSelfClosing = SelfClosing <$> (parseTagOpen *> readUntil ' ') <*> parseTagOptions <* ignoreWhitespace (parseString "/>")

readUntil :: Char -> Parser String
readUntil stop = Parser f where
    f xs | null left = Nothing
         | otherwise = Just (left, remaining)
         where (left, remaining) = break (== stop) xs

readExcluding :: Char -> String -> (String, String) -> (String, String)
readExcluding _ _ (_, []) = ([], [])
readExcluding delim excludes (parsed, current : input)
                            | current `elem` excludes = ([], [])
                            | current == delim = (reverse parsed, current:input)
                            | otherwise = readExcluding delim excludes (current : parsed, input)

isTag :: Char -> Bool
isTag c = c == '<'

satisfy :: (Char -> Bool) -> Parser Char
satisfy v = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | v x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (fi, sitem) = (res, sitem) where
                           res = f fi
second :: (b -> c) -> (a, b) -> (a, c)
second f (fir, sitem) = (fir, res) where
                          res = f sitem
