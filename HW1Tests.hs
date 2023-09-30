{- Example of using the HUnit unit test framework.  See  http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "run" at the Haskell prompt.  -} 

module HW1Tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW1

-- P1(a) count tests  
p1a_test1 = TestCase (assertEqual "count-test1"
                                 3
                                 (count '5' "355-451") )
p1a_test2 = TestCase (assertEqual "count-test2"
                                 2
                                 (count [] [[],[1,2],[3,2],[5,6,7],[8],[]]) )
p1a_test3 = TestCase (assertEqual "count-test3"
                                 0
                                 (count 0 [1,2,3,4,5,6,7,8,9]) )

p1a_test4 = TestCase (assertEqual "count-test4" 
                                 5
                                 (count 'a' "ababbabaabb") )
p1a_test5 = TestCase (assertEqual "count-test5"
                                 0
                                 (count 10 []) )

-- P1(b) diff tests
p1b_test1 = TestCase (assertEqual "diff-test1"
                                 (sort [6,6,6,6,6,6])
                                 (sort $ diff [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6] [1,2,3,4,5,7,7]) )
p1b_test2 = TestCase (assertEqual "diff-test2"
                                 [7]
                                 (sort $ diff [1,2,2,3,3,3,6,7,4,4,4,4,5,5,5,5,5,6,6] [1,1,2,2,3,3,4,4,5,6,6,6]) )
p1b_test3 = TestCase (assertEqual "diff-test3"
                                 []
                                 (sort $ diff [6,2,2,3,5,3,6,7,4,4,5,4,5,5,4,5,3,1,6] [1,2,2,3,3,3,6,7,4,4,4,4,5,5,5,5,5,6,6]) )

p1b_test4 = TestCase (assertEqual "diff-test4"
                                 (sort [1,2,2,3,3,3,6,7,4,4,4,4,5,5,5,5,5,6,6])
                                 (sort $ diff [1,2,2,3,3,3,6,7,4,4,4,4,5,5,5,5,5,6,6] []) )
p1b_test5 = TestCase (assertEqual "diff-test5"
                                 "c"
                                 (sort $ diff "abac" "aba") )


-- P1(c) bag_diff tests
p1c_test1 = TestCase (assertEqual "bag_diff-test1"
                                 (sort [2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6])
                                 (sort $ bag_diff [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6] [1,2,3,4,5,7,7]) )
p1c_test2 = TestCase (assertEqual "bag_diff-test2"
                                 (sort [3,4,4,5,5,5,5,7])
                                 (sort $ bag_diff [1,2,2,3,3,3,6,7,4,4,4,4,5,5,5,5,5,6,6] [1,1,2,2,3,3,4,4,5,6,6,6]) )
p1c_test3 = TestCase (assertEqual "bag_diff-test3"
                                 []
                                 (sort $ bag_diff [6,2,2,3,5,3,6,7,4,4,5,4,5,5,4,5,3,1,6] [1,2,2,3,3,3,6,7,4,4,4,4,5,5,5,5,5,6,6]) )
p1c_test4 = TestCase (assertEqual "bag_diff-test4"
                                 (sort "testing myuncon")
                                 (sort $ bag_diff "testing my function" "fit ") )

p1c_test5 = TestCase (assertEqual "bag_diff-test5"
                                 (sort "")
                                 (sort $ bag_diff [] []) )
p1c_test6 = TestCase (assertEqual "bag_diff-test6"
                                 (sort "123456789")
                                 (sort $ bag_diff "12345" "6789") )

-- P2  everyN tests
p2_test1 = TestCase (assertEqual "everyN-test1"
                                  [3,6,9,12]
                                  (everyN [1,2,3,4,5,6,7,8,9,10,11,12,13] 3) )
p2_test2 = TestCase (assertEqual "everyN-test2"
                                  "HASKELL"
                                  (everyN "hHaAsSkKeElLlL" 2))
p2_test3 = TestCase (assertEqual "everyN-test3"
                                  ([]::[Int])
                                  (everyN [] 5) )
p2_test4 = TestCase (assertEqual "everyN-test4"
                                  "haskell"
                                  (everyN "haskell" 1) )

p2_test5 = TestCase (assertEqual "everyN-test5"
                                  ""
                                  (everyN "abcdefg" 0) )
p2_test6 = TestCase (assertEqual "everyN-test6"
                                  "a"
                                  (everyN "a" 1) )

-- P3(a) make_sparse tests
p3a_test1 = TestCase (assertEqual "make_sparse-test1"
                                  [0,0,0,30,0,0,0,0,0,0,100,110]
                                  (make_sparse [(3,30),(10,100),(11,110)]) )
p3a_test2 = TestCase (assertEqual "make_sparse-test2"
                                  [0,1,2,0,4,0,6,0,0,9]
                                  (make_sparse [(1,1),(2,2),(4,4),(6,6),(9,9)]) )
p3a_test3 = TestCase (assertEqual "make_sparse-test3"
                                  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
                                  (make_sparse [(20,1)]) )
p3a_test4 = TestCase (assertEqual "make_sparse-test4"
                                  ([]::[Int])
                                  (make_sparse []) )

p3a_test5 = TestCase (assertEqual "make_sparse-test5"
                                  [5]
                                  (make_sparse [(0,5)]) )
p3a_test6 = TestCase (assertEqual "make_sparse-test6" 
                                  [0]
                                  (make_sparse [(0,0)]) )

-- P3(b) compress tests
p3b_test1 = TestCase (assertEqual "compress-test1"
                                  [(3,30),(10,100),(11,110)]
                                  (compress [0,0,0,30,0,0,0,0,0,0,100,110]) )
p3b_test2 = TestCase (assertEqual "compress-test2"
                                  [(1,1),(2,2),(4,4),(6,6),(9,9)]
                                  (compress [0,1,2,0,4,0,6,0,0,9]) )
p3b_test3 = TestCase (assertEqual "compress-test3"
                                  [(20,1)]
                                  (compress [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]) )
p3b_test4 = TestCase (assertEqual "compress-test4"
                                  ([]::[(Int,Int)])
                                  (compress []) )

p3b_test5 = TestCase (assertEqual "compress-test5"
                                  [(0,5)]
                                  (compress [5]) )
p3b_test6 = TestCase (assertEqual "compress-test6"
                                  []
                                  (compress [0]) )                            

-- P4 added_sums tests
p4_test1 = TestCase (assertEqual "added_sums-test1"
                                  ([1,3,6,10,15,21,28,36,45,55])
                                  (added_sums [1,2,3,4,5,6,7,8,9,10]) )
p4_test2 = TestCase (assertEqual "added_sums-test2"
                                  ([0,-2,1,5,1,0,2])
                                  (added_sums [0,-2,3,4,-4,-1,2]) )
p4_test3 = TestCase (assertEqual "added_sums-test3"
                                  ([])
                                  (added_sums []) )

p4_test4 = TestCase (assertEqual "added_sums-test4"
                                  ([5,0])
                                  (added_sums [5,-5]) )
p4_test5 = TestCase (assertEqual "added_sums-test5"
                                  ([0,-5,-2,0,-4,-7,0])
                                  (added_sums [0,-5,3,2,-4,-3,7]) )

-- P5 find_routes tests
routes_test = [("Lentil",["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main", "Arbor", "Sunnyside", "Fountain", "Crestview", "Wheatland", "Walmart", "Bishop", "Derby", "Dilke"]), 
   ("Wheat",["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay", "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"]), 
   ("Silver",["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Outlet", "RockeyWay","Main"]),
   ("Blue",["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell", "Chinook", "Library"]),
   ("Gray",["TransferStation", "Wawawai", "Main", "Sunnyside","Crestview", "CityHall", "Stadium", "Colorado"]),
   ("Coffee",["TransferStation", "Grand", "Main", "Visitor","Stadium", "Spark", "CUB"])]    

routes_test2 = [("Route A", ["Somewhere","Everywhere","Elsewhere","Someplace"]),
                ("Route B", ["Here","There","Around","Near","Adjacent"])]

p5_test1 = TestCase (assertEqual "find_routes-test1"
                                  (sort ["Lentil","Wheat","Silver"])
                                  (sort $ find_routes "Walmart" routes_test ) )
p5_test2 = TestCase (assertEqual "find_routes-test2"
                                  ([])
                                  (sort $ find_routes "Rosauers" routes_test ) )
p5_test3 = TestCase (assertEqual "find_routes-test3"
                                  (sort ["Lentil","Silver","Gray","Coffee"])
                                  (sort $ find_routes "Main" routes_test ) )

p5_test4 = TestCase (assertEqual "find_routes-test4"
                                  (sort [])
                                  (sort $ find_routes "Route A" routes_test2 ) )


-- P6 group_sum tests
p6_test1 = TestCase (assertEqual "(group_sum-test1)"
                                  [[1,2,3,4],[5,6,7],[8,9,10,11],[12,13,14,15,16],[17]] 
                                  (group_sum [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17] 10) )
p6_test2 = TestCase (assertEqual "(group_sum-test2)"
                                  [[],[12],[10,1,3,4,7,11],[22,2,5,40],[100,4]]
                                  (group_sum [12,10,1,3,4,7,11,22,2,5,40,100,4] 10) )
p6_test3 = TestCase (assertEqual "(group_sum-test3)"
                                  [[],[5,-2,-3,4,-5,6],[7,8],[9,10,11],[12,-13,14,15,16,20]]
                                  (group_sum [5,-2,-3,4,-5,6,7,8,9,10,11,12,-13,14,15,16,20] 4) )

p6_test4 = TestCase (assertEqual "(group_sum-test4)"
                                  [[0,0,0,0]]
                                  (group_sum [0,0,0,0] 1) )
p6_test5 = TestCase (assertEqual "(group_sum-test5)"
                                  [[5],[1,2,2]]
                                  (group_sum [5,1,2,2] 5) )

-- add the test cases you created to the below list. 
tests = TestList [ TestLabel "Problem 1a- test1 " p1a_test1,
                   TestLabel "Problem 1a- test2 " p1a_test2,
                   TestLabel "Problem 1a- test3 " p1a_test3,
                   TestLabel "Problem 1a- test4 " p1a_test4,
                   TestLabel "Problem 1a- test5 " p1a_test5,

                   TestLabel "Problem 1b- test1 " p1b_test1,
                   TestLabel "Problem 1b- test2 " p1b_test2,
                   TestLabel "Problem 1b- test3 " p1b_test3,
                   TestLabel "Problem 1b- test4 " p1b_test4,
                   TestLabel "Problem 1b- test5 " p1b_test5,

                   TestLabel "Problem 1c- test1 " p1c_test1,
                   TestLabel "Problem 1c- test2 " p1c_test2,
                   TestLabel "Problem 1c- test3 " p1c_test3,
                   TestLabel "Problem 1c- test4 " p1c_test4,
                   TestLabel "Problem 1c- test5 " p1c_test5,
                   TestLabel "Problem 1c- test6 " p1c_test6,

                   TestLabel "Problem 2- test1 " p2_test1,
                   TestLabel "Problem 2- test2 " p2_test2,
                   TestLabel "Problem 2- test3 " p2_test3,
                   TestLabel "Problem 2- test4 " p2_test4,
                   TestLabel "Problem 2- test5 " p2_test5,
                   TestLabel "Problem 2- test6 " p2_test6,

                   TestLabel "Problem 3a- test1 " p3a_test1,
                   TestLabel "Problem 3a- test2 " p3a_test2,
                   TestLabel "Problem 3a- test3 " p3a_test3,
                   TestLabel "Problem 3a- test4 " p3a_test4,
                   TestLabel "Problem 3a- test5 " p3a_test5,
                   TestLabel "Problem 3a- test6 " p3a_test6,

                   TestLabel "Problem 3b- test1 " p3b_test1,
                   TestLabel "Problem 3b- test2 " p3b_test2,
                   TestLabel "Problem 3b- test3 " p3b_test3,
                   TestLabel "Problem 3b- test4 " p3b_test4,
                   TestLabel "Problem 3b- test5 " p3b_test5,
                   TestLabel "Problem 3b- test6 " p3b_test6,

                   TestLabel "Problem 4- test1 " p4_test1,
                   TestLabel "Problem 4- test2 " p4_test2,
                   TestLabel "Problem 4- test3 " p4_test3,
                   TestLabel "Problem 4- test4 " p4_test4,
                   TestLabel "Problem 4- test5 " p4_test5,

                   TestLabel "Problem 5- test1 " p5_test1,
                   TestLabel "Problem 5- test2 " p5_test2,
                   TestLabel "Problem 5- test3 " p5_test3,
                   TestLabel "Problem 5- test4 " p5_test4,

                   TestLabel "Problem 6- test1 " p6_test1,
                   TestLabel "Problem 6- test2 " p6_test2,
                   TestLabel "Problem 6- test3 " p6_test3,
                   TestLabel "Problem 6- test4 " p6_test4,
                   TestLabel "Problem 6- test5 " p6_test5
                 ] 
                  
-- shortcut to run the tests
run = runTestTT  tests