module MP (separators, lookUp, splitText, combine, getKeywordDefs, expand) where

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

{-|
This function will look up a key in a list of key-value pairs,
returning all the values that match with that key.

> lookUp "A" [("A", 8), ("B", 9), ("C", 5), ("A", 7)] == [8, 7]
-}
lookUp :: String -> [(String, a)] -> [a]
lookUp _ [] = []
lookUp str ((key, val):xs) = [val | key == str] ++ lookUp str xs 

{-|
This function will break up a string with some given separator
characters, returning both the list of separators found between
each "word" and the words themselves.
-}

-- 
splitText :: [Char] -- ^ the separators to split on
          -> String -- ^ the string to split
          -> ([Char], [String])
splitText _ [] = ([], [""])
splitText seps (x:xs)
 | elem x seps = (x:foundSep, "":w:ws)
 | otherwise = (foundSep, (x:w):ws)
  where
    (foundSep, w:ws) = splitText seps xs


{-|
This function interleaves the characters from the first argument
list with the strings in the second argument. The second list must
be non-empty.
-}
combine :: [Char] -> [String] -> [String]
combine "" a = a
combine (x:xs) (y:ys) = y:[x]:combine xs ys 

{-|
This function takes a list of lines and splits each line to
extract a list of keyword-definition pairs.

> getKeywordDefs ["$x Define x", "$y 55"] == [("$x", "Define x"), ("$y", "55")]
-}
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs ("":xs) = getKeywordDefs xs
getKeywordDefs (x:xs) = (key, defin):getKeywordDefs xs
 where
    (_, key:rest) = splitText " " x
    defin = unwords rest


-- getKeywordDefs :: [String] -> KeywordDefs
-- getKeywordDefs [] = []
-- getKeywordDefs (x:xs) = (key, def):getKeywordDefs xs
--  where
--     (key:remains) = words x
--     def = unwords remains

replaceWords :: KeywordDefs -> [String] -> String
replaceWords _ [] = ""
replaceWords [] s = concat s
replaceWords defs wordlist = concat (map replace wordlist)
 where
    replace word = helperf word (lookUp word defs)
    helperf w [] = w
    helperf _ (x:_) = x


{-|
This function takes the contents of two files, one containing
a template and the other the definitions for each keyword
found within the template. It extracts the keyword-definition
information from the info file and uses it to expand the occurrences
of these keywords in the template file, producing new file contents
as a result.

> expand "The capital $1 is $2" "$1 Peru\n$2 Lima." == "The capital of Peru is Lima"
-}
expand :: FileContents -- ^ the template file contents
       -> FileContents -- ^ the info file contents
       -> FileContents
expand "" _ = ""
expand template info = replaceWords defs (combine seps wordlist)
 where
    defs = getKeywordDefs (lines info)
    (seps, wordlist) = splitText separators template
