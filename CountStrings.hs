import Test.HUnit
import qualified Data.List as List

main :: IO ()
main = do
  runTestTT $ TestList [
    testListPossibilities
    ]
  putStrLn $ show $ possibilities
  putStrLn $ show $ length possibilities where
  possibilities = ["Not implemented"]
  {- possibilities = listPossibilities (RE "((ab)|(ba))") 2-}

data RegularExpression = Symbol Char 
  | Concat RegularExpression RegularExpression 
  | Union RegularExpression RegularExpression
  | Star RegularExpression
  deriving (Show, Eq)

countPossibilities :: RegularExpression -> Int -> Int
countPossibilities re len = length $ listPossibilities re len

listPossibilities :: RegularExpression -> Int -> [String]

testListPossibilities = "Test listPossibilities" ~: TestList [
  testListPossibilitiesSymbol,
  testListPossibilitiesConcat,
  testListPossibilitiesStar,
  testListPossibilitiesUnion,
  testListPossibilitiesComplex
  ]

listPossibilities (Symbol c) limit
  | limit > 0 = [[c]]
  | otherwise = []

listPossibilities (Concat r1 r2) limit = combos where
  o1 = listPossibilities r1 limit
  o2 = listPossibilities r2 limit
  combos = [ a ++ b | a <- o1, b <- o2, length (a++b) <= limit ]

listPossibilities (Star _) 0 = [""]
listPossibilities (Star r1) limit = possibilities where
  opt = listPossibilities r1 limit
  possibilities = "": whileLimit opt limit opt

  -- | Uses nub right now, really inefficient. probably should use some
  -- sort of memoization
  whileLimit :: [String] -> Int -> [String] -> [String]
  whileLimit base lim acc = pos where
    new = List.nub [a++b | a <- base, b <- acc, length (a++b) <= lim]
    pos = if null new || length new < length acc then acc 
          else whileLimit base lim (List.nub $ acc ++ new)

listPossibilities (Union r1 r2) limit = possibilities where
  o1 = listPossibilities r1 limit
  o2 = listPossibilities r2 limit
  possibilities = o1 ++ o2

testListPossibilitiesSymbol :: Test
testListPossibilitiesSymbol = "Test listPossibilities Symbol" ~: TestList [
  -- single letter
  listPossibilities (Symbol 'a') 2 ~?= ["a"],
  listPossibilities (Symbol 'a') 1 ~?= ["a"],
  listPossibilities (Symbol 'a') 0 ~?= [],
  listPossibilities (Symbol 'b') 1 ~?= ["b"]
  ]

testListPossibilitiesConcat :: Test
testListPossibilitiesConcat = "Test listPossibilities Concat" ~: TestList [
  -- concat
  listPossibilities (Concat (Symbol 'a') (Symbol 'b')) 1 ~?= [],
  listPossibilities (Concat (Symbol 'a') (Symbol 'b')) 2 ~?= ["ab"],
  listPossibilities (Concat (Symbol 'a') (Symbol 'b')) 3 ~?= ["ab"]
  ] 

testListPossibilitiesStar :: Test
testListPossibilitiesStar = "Test listPossibilities Star" ~: TestList [
  listPossibilities (Star (Symbol 'a')) 2 ~?= ["", "a", "aa"],
  listPossibilities (Star (Symbol 'b')) 3 ~?= ["", "b", "bb", "bbb"],
  listPossibilities (Star ab) 1 ~?= [""],
  listPossibilities (Star ab) 2 ~?= ["", "ab"],
  listPossibilities (Star ab) 4 ~?= ["", "ab", "abab"],
  listPossibilities (Star ab) 5 ~?= ["", "ab", "abab"]
  ] where 
  a = Symbol 'a'
  b = Symbol 'b'
  ab = Concat a b

testListPossibilitiesUnion :: Test
testListPossibilitiesUnion = "Test listPossibilities Union" ~: TestList [
  listPossibilities (Union a b) 1 ~?= ["a", "b"],
  listPossibilities (Union ab cd) 1 ~?= [],
  listPossibilities (Union ab cd) 2 ~?= ["ab", "cd"]
  ] where
  a = Symbol 'a'
  b = Symbol 'b'
  c = Symbol 'c'
  d = Symbol 'd'
  ab = Concat a b
  cd = Concat c d

testListPossibilitiesComplex :: Test
testListPossibilitiesComplex = "Test listPossibilities Complex" ~: TestList [
  listPossibilities abcd 3 ~?= [],
  listPossibilities abcd 4 ~?= ["abcd"],
  List.sort (listPossibilities (Star abORcd) 4) ~?= List.sort ["", "ab", "cd", "abcd", "abab", "cdab", "cdcd"]
  ] where
  a = Symbol 'a'
  b = Symbol 'b'
  c = Symbol 'c'
  d = Symbol 'd'
  ab = Concat a b
  cd = Concat c d
  abcd = Concat ab cd
  abORcd = Union ab cd

exactPossibilities :: RegularExpression -> Int -> [String]
exactPossibilities re lim = filter (\x -> length x == lim) $ listPossibilities re lim

example :: IO ()
example = do
  putStrLn "((a|b)*) 5"
  putStrLn $ show $ List.sort possibilities 
  putStrLn $ show $ length possibilities 
  putStrLn "((a*)(b(a*))) 100"
  putStrLn $ show $ List.sort pos2
  putStrLn $ show $ length pos2
  where
  possibilities = exactPossibilities (Star aORb) 5 
  a = Symbol 'a'
  b = Symbol 'b'
  aORb = Union a b  
  pos2 = exactPossibilities pat2 100
  pat2 = Concat (Star a) (Concat b (Star a))

