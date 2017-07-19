-- ========================================================================================================
-- Kayhan Dehghani Mohammadi
-- 301243781
-- kdehghan@sfu.ca
-- Assignment 5
-- ========================================================================================================

-- ========================================================================================================
-- P1) The snoc x lst functions returns a new list that is the same as lst
--     except x has been added to the end of it
-- ========================================================================================================
snoc :: a -> [a] -> [a]
snoc x []           = [x]                                    
snoc x (first:rest) = first : (snoc x rest)              -- recursively put x to the rest of the list 

-- ========================================================================================================
-- P2) Write your own version of the Haskell append
-- ========================================================================================================
myappend :: [a] -> [a] -> [a]
myappend [] lst           = lst   
myappend (first:rest) lst = first : (myappend rest lst)  -- recursively append the rest of the elements 

-- ========================================================================================================
-- P3) Write your own version of reverse
-- ========================================================================================================
myreverse :: [a] -> [a]
myreverse []           = []
myreverse (first:rest) = snoc first (myreverse rest)    -- using snoc from P1: put first at the end of list

-- ========================================================================================================
-- P4) Write a function called count_emirps n that returns the number of emirps less than, or equal to, n.
--     An emirp is a prime number that is a different prime when its digits are reversed.
-- ========================================================================================================
count_emirps :: Int -> Int
count_emirps n 
              | n < 13                     = 0
              | (is_prime n) && 
                (is_prime num_reverse) &&
                 num_reverse /= n          = 1 + num_rest_emirps
              | otherwise                  = num_rest_emirps
              where num_reverse            = reverse_int n
                    num_rest_emirps        = count_emirps (n-1)

-----------------------------------------------------------------------------------------------------------
-- helper functions
-----------------------------------------------------------------------------------------------------------
reverse_int :: Int -> Int
reverse_int n = lst_to_int (myreverse (int_to_lst n))

-----------------------------------------------------------------------------------------------------------
int_to_lst :: Int -> [Int]
int_to_lst n 
            | n < 10         = [n]
            | otherwise      = myappend (int_to_lst (n `div` 10)) [remainder]  -- e.g: [119] -> [11] + [9]
            where remainder  = n `rem` 10 

-----------------------------------------------------------------------------------------------------------
lst_to_int :: [Int] -> Int
lst_to_int []                              = 0
lst_to_int (first:rest)
                       | null rest         = first
                       | otherwise         = (first * (10 ^ (length rest))) + (lst_to_int rest)

-----------------------------------------------------------------------------------------------------------
is_prime :: Int -> Bool
is_prime n
          | n < 2                       = False
          | n /= 2 && (n `rem` 2 == 0)  = False        -- skip basic cases for efficiency
          | n /= 3 && (n `rem` 3 == 0)  = False 
          | n /= 5 && (n `rem` 5 == 0)  = False 
          | otherwise                   = check_primality n 2              

-----------------------------------------------------------------------------------------------------------
check_primality :: Int -> Int -> Bool
check_primality n m                             -- check the remainder of n % m for: m from 2 to sqrt (n)
                    | n < (m * m)       = True                  
                    | (n `rem` m) == 0  = False
                    | otherwise         = check_primality n (m + 1)

-- ========================================================================================================
-- P5) Write a function called biggest_sum that takes a list of integer lists as input, and returns the 
--     list with the greatest sum.
-- ========================================================================================================
biggest_sum :: [[Int]] -> [Int]
biggest_sum []                                 = []
biggest_sum [el]                               = el
biggest_sum (first:rest)
                        | head_sum > tail_sum  = first
                        | otherwise            = rest_biggest_sum
                        where head_sum         = sum first
                              rest_biggest_sum = biggest_sum rest
                              tail_sum         = sum rest_biggest_sum

-- ========================================================================================================
-- P6) Write a function called greatest, which returns the item in seq that maximizes function f
-- ========================================================================================================
greatest :: (a -> Int) -> [a] -> a
greatest f []                                      = error "greatest: sequence must be non-empty"
greatest f [el]                                    = el
greatest f (first:rest) 
                       | head_fvalue > tail_fvalue = first
                       | otherwise                 = rest_greatest
                       where head_fvalue           = f first
                             rest_greatest         = greatest f rest
                             tail_fvalue           = f rest_greatest

-- ========================================================================================================
-- P7)  Write a function called is_bit x that returns True when x is 0 or 1, and False otherwise
--      Assume x is of type Int, and the type of the returned value is Bool.
--      Include the most general type signature
-- ========================================================================================================
is_bit :: (Eq a, Num a) => a -> Bool
is_bit x 
        | x == 0 || x == 1  = True
        | otherwise         = False

-- ========================================================================================================
-- P8)  Write a function called flip_bit x that returns 1 if x is 0, and 0 if x is 1. If x is not a bit, 
--      then call error msg
-- ========================================================================================================
flip_bit :: (Eq a, Num a, Num t) => a -> t
flip_bit x
          | not (is_bit x)  = error "flip_bit: bit should be 0 or 1"
          | x == 0          = 1
          | x == 1          = 0

-- ========================================================================================================
-- P9)  Write a function called is_bit_seq<id> x that returns True if x is the empty list, or if it 
--     contains only bits (as determined by is_bit). 
-- ========================================================================================================
-----------------------------------------------------------------------------------------------------------
-- a)   Use recursion and guarded commands.
-----------------------------------------------------------------------------------------------------------
is_bit_seq1 :: (Eq a, Num a) => [a] -> Bool
is_bit_seq1 x
             | null x          = True
             | is_bit (head x) = is_bit_seq1 (tail x)
             | otherwise       = False

-----------------------------------------------------------------------------------------------------------
-- b)   use recursion and at least one if-then-else. No guarded commands.
-----------------------------------------------------------------------------------------------------------
is_bit_seq2 :: (Eq a, Num a) => [a] -> Bool
is_bit_seq2 x = if (null x) then True
                else if (is_bit (head x)) then is_bit_seq2 (tail x)
                else False

-----------------------------------------------------------------------------------------------------------
-- c)   Use the all function in your solution. Don’t use recursion, guarded commands, or if-then-else
-----------------------------------------------------------------------------------------------------------
is_bit_seq3 :: (Foldable t, Eq a, Num a) => t a -> Bool
is_bit_seq3 x = all is_bit x

-- ========================================================================================================
-- P10)  Write a function called invert_bits<id> x that returns a sequence of bits that is the same as x, 
--      except 0s become 1s and 1s become 0s
-- ========================================================================================================
-----------------------------------------------------------------------------------------------------------
-- a)  Use basic recursion.
-----------------------------------------------------------------------------------------------------------
invert_bits1 :: (Eq a1, Num a1, Num a) => [a1] -> [a]
invert_bits1 x 
              | null x         =  []
              | (head x) == 0  =  myappend [1] (invert_bits1 (tail x)) 
              | (head x) == 1  =  myappend [0] (invert_bits1 (tail x))
              | otherwise      =  error "invert_bits1: invalid bit sequence" 

-----------------------------------------------------------------------------------------------------------
-- b)  Use the map function.
-----------------------------------------------------------------------------------------------------------
invert_bits2 :: (Eq a, Num a, Num b) => [a] -> [b]
invert_bits2 x = map flip_bit x

-----------------------------------------------------------------------------------------------------------
-- c)  Use the map function.
-----------------------------------------------------------------------------------------------------------
invert_bits3 :: (Eq a, Num a, Num t) => [a] -> [t]
invert_bits3 x = [flip_bit b | b <- x]

-- ========================================================================================================
-- P11)  Write a function called bit_count x that returns a pair of values indicating the number of 0s 
--       and 1s in x
-- ========================================================================================================
bit_count :: (Eq a, Num t1, Num t, Num a) => [a] -> (t1, t)
bit_count x 
           | null x         = (0, 0)
           | otherwise      = (count_pred (== 0) x, count_pred (== 1) x)

-----------------------------------------------------------------------------------------------------------
-- Helper function to return the number of elements in list for which the predicate holds
-----------------------------------------------------------------------------------------------------------
count_pred :: Num t => (t1 -> Bool) -> [t1] -> t
count_pred pred lst
              | null lst          = 0
              | pred (head lst)   = 1 + count_reset
              | otherwise         = count_reset
              where count_reset   = count_pred pred (tail lst)

-- ========================================================================================================
-- P12)  Write a function called all_bit_seqs n that returns a list of all bit sequences of length n. 
--       The order of the sequences doesn’t matter. If n is less than 1, then return an empty list
-- ========================================================================================================
all_bit_seqs :: (Ord a, Num a, Num t) => a -> [[t]]
all_bit_seqs n
              | n < 1     = []
              | n == 1    = [[0], [1]]
              | otherwise = myappend
                            (append_to_elements 0 rest_all_bits_seqs)
                            (append_to_elements 1 rest_all_bits_seqs)
              where rest_all_bits_seqs = all_bit_seqs (n - 1)

-----------------------------------------------------------------------------------------------------------
-- Helper function to append x to all the elements in the list 
-----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------
append_to_elements :: t -> [[t]] -> [[t]]
append_to_elements x []           = []
append_to_elements x (first:rest) = (myappend [x] first) : (append_to_elements x rest)


-----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------
-- Data Structure used for the following problems
data Bit = Zero | One
    deriving (Show, Eq)
-----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------

-- ========================================================================================================
-- P13)  Implement flipBit :: Bit -> Bit, which changes a Zero to a One, and vice-versa
-- ========================================================================================================
flipBit :: Bit -> Bit
flipBit Zero = One 
flipBit One  = Zero 

-- ========================================================================================================
-- P14)  Implement invert :: [Bit] -> [Bit], which flips all the bits in the input list
-- ========================================================================================================
invert :: [Bit] -> [Bit]
invert lst = map flipBit lst

-- ========================================================================================================
-- P15)  Implement all_bit_seqs n, which returns a list of Bit lists of bit sequences of length n
-- ========================================================================================================
all_Bit_seqs :: (Ord a, Num a) => a -> [[Bit]]
all_Bit_seqs n = map get_bitval_lst $ all_bit_seqs n

-----------------------------------------------------------------------------------------------------------
-- Helper function to apply the get_bitval to a list of bits
-----------------------------------------------------------------------------------------------------------
get_bitval_lst :: [Int] -> [Bit]
get_bitval_lst lst = map get_bitval lst

-----------------------------------------------------------------------------------------------------------
-- Helper function to return Bit value of bit
-----------------------------------------------------------------------------------------------------------
get_bitval :: Int -> Bit
get_bitval 0  = Zero
get_bitval 1  = One

-- ========================================================================================================
-- P16)  Implement bitSum1, which returns the sum of the bits in the input where Zero is 0, and One is 1
--       *** No recursion ***
-- ========================================================================================================
bitSum1 :: [Bit] -> Int
bitSum1 lst = sum $ map get_Bitval1 lst

-----------------------------------------------------------------------------------------------------------
-- Helper function to return the value of Bit
-----------------------------------------------------------------------------------------------------------
get_Bitval1 :: Bit -> Int
get_Bitval1 Zero = 0
get_Bitval1 One  = 1

-- ========================================================================================================
-- P17)  Implement bitSum2 :: [Maybe Bit] -> Int, which returns the sum of the maybe-bits in the input, 
--       i.e. Just Zero is 0, Just One is 1, and Nothing is 0
-- ========================================================================================================
bitSum2 :: [Maybe Bit] -> Int
bitSum2 lst = sum $ map get_Bitval2 lst

-----------------------------------------------------------------------------------------------------------
-- Helper function to return the value of Bit
-----------------------------------------------------------------------------------------------------------
get_Bitval2 :: Maybe Bit -> Int
get_Bitval2 (Just Zero) = 0
get_Bitval2 (Just One)  = 1
get_Bitval2 Nothing     = 0


-----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------
-- Data Structure used for the following problems
data List a = Empty | Cons a (List a)
    deriving Show
-----------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------

-- ========================================================================================================
-- P18)  Implement toList :: [a] -> List a, which converts a regular Haskell list to a List a.
-- ========================================================================================================
toList :: [a] -> List a
toList []           = Empty
toList (first:rest) = Cons first $ toList rest

-- ========================================================================================================
-- P19)  Implement toHaskellList :: List a -> [a], which converts a List a to a regular Haskell list
-- ========================================================================================================
toHaskellList :: List a -> [a]
toHaskellList Empty      = []
toHaskellList (Cons a b) = a : toHaskellList b

-- ========================================================================================================
-- P20)  Implement append A B, that returns a new List a that consists of the elements of A followed by 
--       the elements of B. 
-- ========================================================================================================
append :: List a -> List a -> List a
append Empty s_lst      = s_lst
append (Cons a b) s_lst = Cons a $ append b s_lst