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
myreverse (first:rest) = snoc first (myreverse rest)    -- using snoc from P1: put first at the end of lit

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
              where num_reverse     = reverse_int n
                    num_rest_emirps = count_emirps (n-1)
                    
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
lst_to_int []                               = 0
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

-----------------------------------------------------------------------------------------------------------
-- end of helper functions for part 4
-----------------------------------------------------------------------------------------------------------