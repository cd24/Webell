{-# LANGUAGE GADTs #-}

import           Control.Monad.Catch
import           Control.Monad.Identity
import           Data.Traversable
import           Webell
import           Language.Haskell.Interpreter
import           Parser
import           System.Environment
import           System.IO.Unsafe

push :: String -> (String,String)
push _ = ("a","b")

pull :: String -> String
pull (x:xs) = xs
pull xs = xs

{- This function should handle the parsing -}

parse :: [String] -> String -> String -> String -> Maybe String
parse files (x:xs) (y:ys) consumed = if x == y then parse files xs ys (y:consumed) else
  (case parse files xs openTag "" of
    Nothing -> Nothing
    Just s -> Just (reverse consumed ++ (x : s)))
parse files rest [] consumed = parseClose1 files rest closeTag "" ""
parse files "" _ consumed = Just ""

parseBlock :: Parser String
parseBlock = parseOpen *> parseBody <* parseClose

parseClose1 :: [String] -> String -> String -> String -> String -> Maybe String
parseClose1 files (x:xs) (y:ys) buildup maybs = if x == y then parseClose1 files xs ys buildup (y:maybs) else
    (case parseClose1 files xs closeTag (x : (maybs ++ buildup)) "" of
      Nothing -> Nothing
      Just s -> Just s)
parseClose1 files "" (y:ys) _ _  = Nothing
parseClose1 files rest [] buildup _ = case parse files rest openTag "" of
    Nothing -> Nothing
    Just s -> Just (evalHaskell files (reverse buildup) ++ s)

join mmc = mmc >>= id

openTag = "<hask>"

closeTag = "</hask>"

evalHaskell :: [String] -> String -> String
evalHaskell files s =  unsafePerformIO (evalHaskell' files s :: IO String)

evalHaskell' :: (MonadIO m, MonadMask m) => [String] -> String -> m String
evalHaskell' files s = fmap toHTML (runCode files s)

-- get file, import pairing
imports :: [String] -> [(String, String)]
imports files = [(f, f ++ ".hs") | f <- files]


runCode :: (MonadIO m, Control.Monad.Catch.MonadMask m) =>
           [String] -> String -> m (Either InterpreterError (Tag String))
runCode files code = runInterpreter $ loadModules ("Hasklettes.hs" : file_loc)
                                    >> setImports ("Prelude" : ("Hasklettes" : modules))
                                    >>  interpret code (as :: (Tag String))
                            where
                                (modules, file_loc) = unzip (imports files)

--runExample :: IO (Either InterpreterError (Tag String))
--runExample = runCode "divTag [] []"

main :: IO ()
main = do
    args <- getArgs
    let [in_path, out_path] = take 2 args
    let files = drop 2 args
    file_contents       <- readFile in_path
    let outString       = parse files file_contents openTag ""
    case outString of
      Nothing -> putStrLn "Failed with some error"
      Just s  -> writeFile out_path s
