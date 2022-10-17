module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

--Define separators for the text file
separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

-- Given a string k and list of pairs with a string as the 1st element, 
-- returns the list of the 2nd elements whose 1st element matches k
lookUp :: String -> [(String, a)] -> [a]
lookUp k xys = [y | (x, y) <- xys, k == x]

-- Given a list of separator characters and an input string,
-- returns a tuple with the first element representing the separator characters present
-- in the input string and the second element representing the 
-- input string separated into a list.
splitText :: [Char] -> String -> (String, [String])
splitText seps "" = ("", [""]) -- base case
splitText seps (c : cs)
  | c `elem` seps = (c : a, "" : b) -- if character in string is a separator, create an empty string element
  | otherwise = (a, (c : b1) : bs)
  where
    (a, b) = splitText seps cs
    (b1: bs) = b

-- combine the components of a string from its constituent separator characters 
-- and words, as generated by a call to splitText. This results in a list of strings,
-- that when concatenated will rebuild the original string.
combine :: String -> [String] -> [String]
combine "" s = s
combine k [""] = [k]
combine (k:ks) (s:ss)
  = s : [k] : combine ks ss
     
-- takes the contents of an information file in the form of a list of lines 
-- (each line is a string), and which returns a list of keyword/definition pairs.
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = [] -- base case
getKeywordDefs (line:lines)
  = (kw, combinedkv) : getKeywordDefs lines --recursive step
  where
    (seps, key) = splitText " " line
    (kw : kv) = key -- c1 is the keyword, and c2 is the keyword value, but broken up in a list
    combinedkv = concat (combine ss kv) -- concatenate the remaining words to form the whole keyword value
    ss = if null seps then "" else tail seps -- avoids tailing an empty string

-- takes the contents of a text file and an info file and combines them using the above functions 
-- to build a string representing the output file by replacing keywords with key values
expand :: FileContents -> FileContents -> FileContents
expand ori keystring
  = concat (combine sep replaced_words)
  where 
    (sep, ori_wordlist) = splitText separators ori -- split original text file into words
    keywords = getKeywordDefs(snd(splitText "\n" keystring)) --get keywords from the keyword file
    replaced_words = [replaceWord ori_word keywords | ori_word <- ori_wordlist] --replace keywords with key values

-- replaces a word by a key value given the word and a list of keyword - key value pairs
-- if the word does not match any keywords, it stays the same
replaceWord :: String -> KeywordDefs -> String
replaceWord ori keydefs 
  = if null replaced then ori else head replaced -- if the keyword appears multiple times, take the first occurence
  where
    replaced = lookUp ori keydefs 
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
    main' _ = putStrLn "Usage: runghc MP <template> <info> <output>"
