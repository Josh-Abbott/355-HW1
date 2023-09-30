-- CptS 355 - Spring 2023 -- Homework1 - Haskell
-- Name: Josh Abbott
-- Collaborators: 

module HW1
     where

-- P1(a) count ;  6%
     count :: Eq a => a -> [a] -> Int
     count n [] = 0
     count n (x:xs)
          | n == x = 1 + (count n xs)
          | otherwise = count n xs


-- P1(b) diff ;  6%
     diff :: Eq a => [a] -> [a] -> [a]
     diff [] _ = []
     diff (x:xs) ys
          | elem x ys = diff xs ys
          | otherwise = x : diff xs ys


-- P1(c) bag_diff ; 8%
     bag_diff :: Eq a => [a] -> [a] -> [a]
     bag_diff [] _ = []
     bag_diff (x:xs) ys
          | count x ys == 0 = x : bag_diff xs ys
          | count x xs > count x ys = x : bag_diff (bag_helper x (count x ys) xs) ys
          | otherwise = bag_diff xs ys

     bag_helper :: Eq a => a -> Int -> [a] -> [a]
     bag_helper _ 0 xs = xs
     bag_helper x n (y:ys)
          | x == y = bag_helper x (n - 1) ys
          | otherwise = y : bag_helper x n ys     


-- P2  everyN ; 10%
     everyN :: [a] -> Int -> [a]
     everyN lst n = everyN_helper lst n n []

     everyN_helper :: [a] -> Int -> Int -> [a] -> [a]
     everyN_helper [] _ _ result = result
     everyN_helper (x:xs) n count result
          | count == 1 = everyN_helper xs n n (result ++ [x])
          | otherwise = everyN_helper xs n (count - 1) result


-- P3(a) make_sparse ; 15%
     make_sparse :: [(Int, Int)] -> [Int]
     make_sparse [] = []
     make_sparse xs = sparse_helper xs 0 []
          where
               sparse_helper [] _ res = reverse res
               sparse_helper ((a, b):xs) n res
                    | a == n = sparse_helper xs (n + 1) (b:res)
                    | otherwise = sparse_helper ((a, b):xs) (n + 1) (0:res)


-- P3(b) compress ; 15%
     compress :: [Int] -> [(Int, Int)]
     compress lst = compress_helper 0 lst
          where
               compress_helper _ [] = []
               compress_helper i (x:xs)
                    | x == 0 = compress_helper (i+1) xs
                    | otherwise = (i, x) : compress_helper (i+1) xs


-- P4 added_sums ; 8%
     added_sums :: [Int] -> [Int]
     added_sums [] = []
     added_sums xs = reverse (sums_helper xs 0 [])
          where sums_helper [] _ acc = acc
                sums_helper (x:xs) sum acc = sums_helper xs (sum + x) (sum + x : acc)


-- P5 find_routes ; 8%
     find_routes :: Eq a => a -> [(b, [a])] -> [b]
     find_routes loc [] = [] 
     find_routes loc (y:ys) = if(elem loc (snd y)) 
                                   then ((fst y):(find_routes loc ys)) 
                                   else (find_routes loc ys)


-- P6 group_sum ; 15%
     group_sum :: [Int] -> Int -> [[Int]]
     group_sum lst n = group_sum' lst n 0
          where group_sum' [] _ _ = []
                group_sum' lst n k = let (group, rest) = sum_helper (n * 2^k) lst
                    in group : group_sum' rest n (k+1)

     sum_helper :: Int -> [Int] -> ([Int], [Int])
     sum_helper limit xs = sum_helper' limit xs []
          where sum_helper' _ [] acc = (reverse acc, [])
                sum_helper' limit (x:xs) acc = if x <= limit
                    then sum_helper' (limit - x) xs (x:acc)
                    else (reverse acc, x:xs)


-- Assignment rules ; 3%
-- Your own tests; please add your tests to the HW1Tests.hs file ; 6%



