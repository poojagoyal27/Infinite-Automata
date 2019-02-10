-- CSci 119, Lab 5
-- Reference: Lecture notes, Sections 4.1, 4.2

-----Pooja Goyal----Student Id---109896598
import Data.List (nub, sort, subsequences)


sigma = ['a', 'b']

-- Finite State Machine M = (Q, q0, F, d)
type FSM = ([Int], Int, [Int], [(Int,Char,Int)])

atleastonea::FSM
atleastonea=([0,1],0,[1],[(0,'a',1),(0,'b',0),(1,'a',1),(1,'b',1)])


-- Check whether a finite state machine (qs, q0, fs, d) is correct/complete:
-- (1) States qs are unique (no duplicates)
-- (2) Start state is a state (q0 is in qs)
-- (3) Final states are states (fs is a subset of qs; don't worry about dups)
-- (4) Transition relation is a function from qs and sigma to qs (exactly one
--     output state for each state and letter from sigma)

--no_dups = "rs has no duplicates it is unique"
no_dups :: [Int] -> Bool
no_dups [] = True   
no_dups (x:xs) = not (elem x xs) && no_dups xs


start_state :: Int -> [Int] -> Bool
start_state x rs = x `elem` rs

final_state :: [Int] -> [Int] -> Bool
final_state fs qs = and [ elem x qs | x <- fs]

check_trans :: [(Int, Char, Int)] -> [Int] -> Bool
check_trans _ [] = False
check_trans [] _ = True 
check_trans (t@(s1, c, s2):ts) states = elem s1 states && elem c sigma && elem s2 states && 
                                        and[s2==s2'| (s1,c,s2) <- (t:ts), (s1',c',s2')<-(t:ts), c == c', s1 == s1'] && check_trans ts states

checkFSM :: FSM -> Bool
checkFSM (qs, q0, fs, ts) = (no_dups qs) && (start_state q0 qs) && (final_state fs qs) && (check_trans ts qs)

-- Gives the transition function of the machine as a function
-- i.e., delta m q a = the state machine m goes to when reading a in state q
delta :: FSM -> Int -> Char -> Int
delta (qs, q0, fs, ts) q a = minimum [ z | (x, y, z) <- ts, q == x, y == a]


-- Gives the delta* function
delta_star :: FSM -> Int -> [Char] -> Int
delta_star m q "" = q
delta_star m q (a:w) = delta_star m (delta m q a) w

accept1_final_state :: FSM -> [Int]
accept1_final_state (qs, q0, fs, ts) = fs

accept1_start_state :: FSM -> Int
accept1_start_state (qs, q0, fs, ts) = q0

-- Machine acceptance, Definition 1 (via delta*)
accept1 :: FSM -> [Char] -> Bool
accept1 m w = (checkFSM m) && ((delta_star m (accept1_start_state m) w) `elem` (accept1_final_state m))

-- Machine acceptance, Definition 2 (via L_q(M))

accept2_aux_fs :: FSM -> Int -> Bool
accept2_aux_fs (qs, q0, fs, ts) q = q `elem` fs


accept2_aux :: FSM -> Int -> [Char] -> Bool
accept2_aux m q "" = True && checkFSM m && (accept2_aux_fs m q)
accept2_aux m q (a:w) = accept2_aux m (delta m q a) w

accept2_ss :: FSM -> Int
accept2_ss (qs, q0, fs, ts) = q0

-- Defined (non-recursively) in terms of accept2_aux
accept2 :: FSM -> [Char] -> Bool
accept2 m w = accept2_aux m (accept2_ss m) w

-- Define a machine that accepts exactly the strings with an even number of a's
-- and test it adequately

even_as = ([0..3], 0, [2], [(0,'a',1),(1,'a',2),(2,'a',1),(2,'b',2),(0,'b',3),(3,'a',1),(1,'b',1)])


-- Define a machine that accepts exactly the strings that do not contain "aaa"
-- as a substring and test it adequately

no_aaa = ([1..4],1,[1..3],[(1,'a',2),(1,'b',1),(2,'a',3),(2,'b',1),(3,'a',4),(3,'b',1)])



