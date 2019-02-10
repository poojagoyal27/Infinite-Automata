-- CSci 119, Lab 4
-----Pooja Goyal-----Student ID---109896598--------

---- Regular expressions, along with input and output
data RegExp = Empty
             | Letter Char
             | Union RegExp RegExp
             | Cat RegExp RegExp
             | Star RegExp

instance (Show RegExp) where    -- use precedence to minimize parentheses
  showsPrec d Empty         = showString "@"
  showsPrec d (Letter c)    = showString [c]
  showsPrec d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                              showsPrec 6 r1 .
                              showString "+" .
                              showsPrec 6 r2
  showsPrec d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                              showsPrec 7 r1 .
                              showsPrec 7 r2
  showsPrec d (Star r1)     = showsPrec 9 r1 .     -- prec(Star) = 8
                              showString "*"

-- Quick and dirty postfix regex parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = toRE' w [] where
  toRE' [] [r] = r
  toRE' ('+':xs) (r2:r1:rs) = toRE' xs (Union r1 r2:rs)
  toRE' ('.':xs) (r2:r1:rs) = toRE' xs (Cat r1 r2:rs)
  toRE' ('*':xs) (r:rs) = toRE' xs (Star r:rs)
  toRE' ('@':xs) rs = toRE' xs (Empty:rs)
  toRE' (x:xs) rs = toRE' xs (Letter x:rs)


---------------- Part 1 ----------------

-- Implement the six recursive predications/operations on RegExp given in
-- Section 3.3 of the notes. Each should begin with a type declaration.
-- Include several tests for each function.


-----All predications included in notes with haskell implementation.-----------

--emptiness--A regular expression r is empty if  r  = 0.------ it can be defined as follows:

emptiness :: RegExp -> Bool
emptiness (Empty) = True        ---------empty case----
emptiness (Letter r) = False     ----------letter case-----
emptiness (Union r1 r2) = emptiness(r1) && emptiness(r2)     ----------union case----
emptiness (Cat r1 r2) = emptiness(r1) || emptiness(r2)      --------concatenation case-----
emptiness (Star r1) = False        -----------star case----



--Unitarity. A regular expression r is unitary if  r  = 1. ------ it can be defined as follows:

unitarity :: RegExp -> Bool
unitarity (Empty) = False        -----empty case----1(0) is false---
unitarity (Letter r) = False       ----------letter case----- 1(r) is false
unitarity (Union r1 r2) = (unitarity r1 && unitarity r2) || (emptiness r1 && unitarity r2) || (unitarity r1 && emptiness r2)     ----------union case----1(r1 +r2) iff either 1(r1) and 0(r2), or 0(r1) and 1(r2), or 1(r1) and 1(r2)
unitarity (Cat r1 r2) = (unitarity r1 && unitarity r2)         ----concatenation case-----1(r1r2) iff 1(r1) and 1(r2)----
unitarity (Star r1) = (emptiness r1 || unitarity r1)        ------star case----1(r1∗) iff 0(r1) or 1(r1)-----



--Bypassabiltiy--A regular expression r is bypassable if ε ∈  r  (or, equivalently, 1 ⊆  r ) can be defined as follows:------

bypassabiltiy :: RegExp -> Bool
bypassabiltiy (Empty) = False  -----------empty case----b(0) is false---
bypassabiltiy (Letter a) = False    --------------letter case----- b(a) is false
bypassabiltiy (Union r1 r2) = bypassabiltiy r1 || bypassabiltiy r2   ----------union case--b(r1 + r2) iff b(r1) or b(r2)-------
bypassabiltiy (Cat r1 r2) = bypassabiltiy r1 || bypassabiltiy r2        ----concatenation case----b(r1r2) iff b(r1) and b(r2)---
bypassabiltiy (Star r) = True    ------star case----b(r1∗) is true


--infiniteness--A regular expression r is infinite if  r  is an infinite set. This condition, denoted ∞(r), can be defined as follows:-----


infiniteness :: RegExp -> Bool
infiniteness (Empty) = False   -----------------empty case----∞(0) and ∞(a) are false;
infiniteness (Letter a) = False      --------------letter case-----∞(0) and ∞(a) are false;
infiniteness (Union r1 r2) = infiniteness r1 || infiniteness r2     ----------union case--∞(r1 + r2) iff ∞(r1) or ∞(r2).
infiniteness (Cat r1 r2) = (infiniteness r1 && not (emptiness r2))||(infiniteness r2 && not (emptiness r1))    ----concatenation case---∞(r1r2) iff ∞(r1) and not 0(r2), or ∞(r2) and not 0(r1).
infiniteness (Star r) = not (emptiness r) && not (unitarity r)    ------star case----∞(r1∗) iff not 0(r1) and not 1(r1).


-- reversal--Reversal. By the results of Theorem 2.15, we can define the reversal of a regular expression by---

reversal :: RegExp -> RegExp
reversal (Empty) = Empty    ----------------------empty case----rev(0) = 0
reversal (Letter a) = Letter a           --------------letter case-----rev(a) = a
reversal (Union r1 r2) = (Union (reversal r1) (reversal r2))    ----------union case--rev(r1 + r2) = rev(r1) + rev(r2)
reversal (Cat r1 r2) = (Cat (reversal r1) (reversal r2))      ----concatenation case---rev(r1r2) = rev(r2)rev(r1)
reversal (Star r) = (Star (reversal r))       ------star case----rev(r1∗) = rev(r1)∗


-- left quotient--By the results of Theorem 2.17, we can define the left quotient of a regular expression by a letter s ∈ Σ by

leftquotient :: Char -> RegExp -> RegExp
leftquotient c (Empty) = (Empty)     ----------------------empty case----s\0=0----
leftquotient a1 (Letter a2)          --------------letter case-----s\a=1ifs=a,and0otherwise(here,1=0∗)-----
 | a1 == a2 = (Star (Empty))
 | otherwise = (Empty)
leftquotient a (Union r1 r2) = (Union (leftquotient a r1) (leftquotient a r2))    ----------union case--s\(r1 + r2) = s\r1 + s\r2----
leftquotient a (Cat r1 r2)      ----concatenation case---s\(r1r2) = (s\r1)r2 + s\r2 if b(r1), and (s\r1)r2 otherwise------
 | bypassabiltiy r1 = (Union (Cat (leftquotient a r1) r2) (leftquotient a r2))
 | otherwise = (Cat (leftquotient a r1) r2)
leftquotient a (Star r) = (Cat (leftquotient a r) (Star r))     ------star case----s\(r1∗) = (s\r1)r1-----


---------------- Part 2 ----------------

-- Implement the two matching algorithms given in Section 3.4 of the notes.
-- Call them match1 and match2. Start by implementing splits:

-- splits xs = list of all possible splits of xs, in order. For example,
-- splits "abc" = [("","abc"), ("a","bc"), ("ab","c"), ("abc","")]


--splits :: [a] -> [([a], [a])]
--splits xs = split_helper [] xs
splits :: [a] -> [([a], [a])]
splits xs = [(take x xs, drop x xs) | x <- [0..(length xs)]]



match1 :: RegExp -> String -> Bool
match1 (Empty) w = False
match1 (Letter a) "" = False
match1 (Letter a) (b:w) = a == b && w == []
match1 (Union r1 r2) w = (match1 r1 w) || (match1 r2 w)
match1 (Cat r1 r2) w = or [ (match1 r1 w1) && (match1 r2 w2) | (w1, w2) <- (splits w) ]
match1 (Star r) w = w == "" || or [ w1 /= "" && (match1 r w1) && (match1 (Star r) w2) | (w1, w2) <- (splits w) ]

-------------------------


match2 :: [RegExp] -> String -> Bool

match2 [] w c = w == ""
match2 ((Empty):rs) w c = False
match2 ((Letter a):rs) "" c = False
match2 ((Letter a):rs) (b:w) c = (a:w) == (b:w) && (match2 rs w False)
match2 ((Union r1 r2):rs) w c = (match2 (r1:rs) w c) || (match2 (r2:rs) w c)
match2 ((Cat r1 r2):rs) w c = (match2 (r1:r2:rs) w c) || ( c == True && bypassabiltiy(r1) && (match2 (r2:rs) w True) )
match2 ((Star r):rs) w c = (c == False && (match2 rs w False)) || (match2 (r:rs) w True)




-- Some regular expressions for testing. Also, test them on other solutions
-- to the exercises of Section 3.2 (as many as you can get).

ab   = toRE "aa.bb.+*"            -- every letter is duplicated
ttla = toRE "ab+*a.ab+.ab+."      -- third to last letter is a
ena  = toRE "b*a.b*.a.*b*."       -- even number of a's
bb1  = toRE "aba.+*b.b.aab.+*."   -- contains bb exactly once
