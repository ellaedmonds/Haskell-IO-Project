{-
    Write and submit a Haskell program (distribution.hs) that computes and displays 
    the distribution of characters in a given sample of text.
    
    Output of your program should look like this:
    
    Please enter a string of text (the bigger the better): 
    The rain in Spain stays mainly in the plain.
    The distribution of characters in "The rain in Spain stays mainly in the plain." is:
    iiiiii
    nnnnnn
    aaaaa
    sss
    ttt
    ee
    hh
    ll
    pp
    yy
    m
    r
    
    Notice about this example:
    * The text: 'The rain ... plain' is provided by the user as input to your program.
    * Uppercase characters are converted to lowercase
    * Spaces and punctuation marks are ignored completely.
    * Characters that are more common appear first in the list.
    * Where the same number of characters occur, the lines are ordered alphabetically. 
      For example, in the printout above, the letters e, h, l, p and y both occur twice 
      in the text and they are listed in the output in alphabetical order.
    * Letters that do not occur in the text are not listed in the output at all.
-}

import Data.List

import Data.Char (toLower)

filteredletters n = filter (`elem`['a'..'z']) n

sortedletters n = sort (filteredletters n)

groupletters n = group $ sortedletters n

showlengths n = map length $ groupletters n

lengthgroupzip n = zip (showlengths n) (groupletters n)

sortedzip n = sort $ reverse $ lengthgroupzip n

justletters n = map (\(a,b) -> b) (sortedzip n) 

lengsortletter n = sortBy (\x y -> length y `compare` length x) $ justletters n

main = do 
  putStrLn "Please enter a string of text (the bigger the better): "
  n <- getLine
  let m = map toLower n
  putStrLn ("The distribution of characters in "++show n++" is:")
  mapM_ putStrLn (lengsortletter m)