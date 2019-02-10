----Pooja---Goyal---student ID----109896598----

import Data.List (nub)


---- String operations (note: String = [Char])

-- Length and concatenation (Def 2.2, reimplements len and ++)
strlen :: String -> Int
strlen "" = 0
strlen (x:xs) = 1 + (strlen xs)

strcat :: String -> String -> String
strcat xs "" = ""
strcat "" ys = ys
strcat (x:xs) ys = x:(strcat xs ys)


---- Language operations. Assume inputs contain no duplicates, and insure that
---- outputs also contain no duplicates

type Language = [String]

-- Union of languages
union_lang :: Language -> Language -> Language
union_lang l1 [] = l1
union_lang [] l2 = l2
union_lang (l1:ls) l2 = if elem l1 l2 then (union_lang ls l2) else l1:(union_lang ls l2)

-- Concatenation of languages (Def 2.5)
concat_lang :: Language -> Language -> Language
concat_lang [] l2 = []
concat_lang l1 [] = l1
concat_lang l1 l2 = nub([strcat w1 w2 | w1 <-l1, w2<-l2])

-- L^n = L * L * ... * L (n times)
power_lang :: Language -> Int -> Language
power_lang l 0 =[""]
power_lang [] n = [""]
power_lang l 1 = l
power_lang l n = concat_lang l (power_lang l (n-1))


-- Bounded Kleene star, L* = L^0 U L^1 U L^2 U ... U L^n
bstar_lang :: Language -> Int -> Language
bstar_lang [] n = [""]
bstar_lang l 0 = [""]
bstar_lang l n = union_lang  (power_lang l n) (bstar_lang l (n-1))

-- Reimplements elem for Languages
element :: String -> Language -> Bool
element _ [] = False
element x (l:ls) = x==l || (element x ls)


-- L1 subset L2
subset :: Language -> Language -> Bool
subset [] l2 = False
subset l2 [] = False
subset l1 l2 = and [element x l2 |x <- l1]



---- Regular expressions and the languages they denote 
data RegExp = Empty
             | Str String
             | Cat RegExp RegExp
             | Union RegExp RegExp
             | Star RegExp

-- [[r]], except use bound 5 for Kleene star
lang_of :: RegExp -> Language
lang_of (Empty) = [] --case Empty
lang_of (Str r) = [r] --case Str String
lang_of (Cat r1 r2) = concat_lang (lang_of r1) (lang_of r2)  -- case Cat RegExp RegExp
lang_of (Union r1 r2) = union_lang (lang_of r1) (lang_of r2) -- case  Union RegExp RegExp
lang_of (Star  r1) = bstar_lang (lang_of r1) 5 --case  Star RegExp
              
