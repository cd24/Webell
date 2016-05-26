module Parser where
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Webell

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

ignoreWhitespace :: Parser a -> Parser a
ignoreWhitespace after = many ((char ' ' <|> char '\t') <|> char '\n') *> after

parseString :: String -> Parser String
parseString phrase = sequenceA (satisfy . (==) <$> phrase)

parseOpen :: Parser String
parseOpen = parseString "<hask>"

parseClose :: Parser String
parseClose = parseString "</hask>"

parseBody :: Parser String
parseBody = Parser f where
    f xs | null ns = Nothing
         | otherwise = Just (ns, remaining)
         where (ns, remaining) = break isTag xs


parseLone :: Parser TagOption
parseLone = Parser f where
    f xs  | null op = Nothing
          | otherwise = Just (Lone op, remaining)
          where (op, remaining) = readExcluding ' ' ['='] ("", xs)

parseTO :: Parser TagOption
parseTO = (\left _ right -> TO left right) <$> readUntil '=' <*> char '=' <*> (readUntil ' ' <|> readUntil '>')

parseTagOption :: Parser TagOption
parseTagOption = ignoreWhitespace (parseTO <|> parseLone)

parseTagOptions :: Parser [TagOption]
parseTagOptions = many parseTagOption

constructTag :: (HTML a) => String -> [TagOption] -> [Tag a] -> Tag a
constructTag = Tag

-- parseTag :: Parser (Tag a)
parseTag = char '<' *>
                    readUntil ' ' <*>
                    parseTagOptions <*>
                    pure [] <*
                    char '>'

readUntil :: Char -> Parser String
readUntil stop = Parser f where
    f xs | null left = Nothing
         | otherwise = Just (left, remaining)
         where (left, remaining) = break (== stop) xs

readExcluding :: Char -> [Char] -> (String, String) -> (String, String)
readExcluding _ _ (_, [])                          = ([], [])
readExcluding delim excludes (parsed, (current:input)) =
              if elem current excludes then ([], [])
              else ( if current == delim then (reverse parsed, input)
                     else readExcluding delim excludes ((current:parsed), input)
              )


--parseTag :: Parser (Tag String)
--parseTag

--parseTag :: Parser (Tag String)
--parseTag = char '<' *> (parseTag <|> parseBody) <* char '>'

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

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ =  c2 <$> char 'a' <*> char 'b'

c2 :: a -> a -> ()
c2 _ _ = ()

intPair :: Parser [Integer]
intPair = c4 <$> (c3 <$> posInt <*> char ' ') <*> posInt

c3 :: Integer -> Char -> Integer
c3 l _ = l

c4 :: Integer -> Integer -> [Integer]
c4 l b = [l, b]

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser f
    where
      f xs = case runParser p1 xs of
        Just l -> Just l
        Nothing -> runParser p2 xs

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
