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

-- Given a string k and list of pairs with a string as the 1st element, 
-- returns the list of the 2nd elements whose 1st element matches k
lookUp :: String -> [(String, a)] -> [a]
lookUp k xys = [y | (x, y) <- xys, k == x]

splitText :: [Char] -> String -> (String, [String])
splitText ks "" = ("", [""])
splitText ks (c : cs)
  | c `elem` ks = (c : a, "" : b)
  | otherwise = (a, (c : b1) : bs)
  where
    (a, b) = splitText ks cs
    (b1: bs) = b

combine :: String -> [String] -> [String]
combine "" s = s
combine k [""] = [k]
combine (k:ks) (s:ss)
  = s : [k] : combine ks ss
     

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (s:ss)
  = (c1, a) : getKeywordDefs ss
  where
    (k, cs) = splitText separators s
    (c1 : c2) = cs
    a = concat (combine (tail k) c2)

expand :: FileContents -> FileContents -> FileContents
expand ori keystring
  = concat (combine sep rep_words)
  where 
    (sep, ori_words) = splitText separators ori
    keywords = getKeywordDefs(snd(splitText "\n" keystring))
    rep_words = [replaceWord ori_word keywords | ori_word <- ori_words]

-- You may wish to uncomment and implement this helper function
-- when implementing expand
replaceWord :: String -> KeywordDefs -> String
replaceWord "" keydefs = ""
replaceWord ori keydefs 
  = if null replaced then ori else head replaced
  where
    replaced = [v | (d, v) <- keydefs, ori == d]
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
