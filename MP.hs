module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp k [] = []
lookUp k ((x, y) : xys) 
  = if k == x 
      then 
        y: lookUp k xys 
      else 
        lookUp k xys

splitText :: [Char] -> String -> (String, [String])
splitText
  = undefined

combine :: String -> [String] -> [String]
combine
  = undefined

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs
  = undefined

expand :: FileContents -> FileContents -> FileContents
expand
  = undefined

-- You may wish to uncomment and implement this helper function
-- when implementing expand
-- replaceWord :: String -> KeywordDefs -> String

-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
