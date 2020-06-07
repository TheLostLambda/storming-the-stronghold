import Data.Foldable
import Data.List
import qualified Data.Map as M

-- Generic Helper Functions
count :: (Foldable f, Eq a) => a -> f a -> Int
count item = length . filter (== item) . toList

frequencies :: (Foldable f, Eq a, Ord a) => f a -> [(a, Int)]
frequencies = map (\x -> (head x, length x)) . group . sort . toList

replace :: (Eq a, Functor f) => a -> a -> f a -> f a
replace a b = fmap (\x -> if x == a then b else x)

-- Counting DNA Nucleotides
countBases0 :: String -> [Int]
countBases0 = map length . group . sort

countBases1 :: String -> [Int]
countBases1 = (map count "ACGT" <*>) . pure

countBases2 :: String -> [Int]
countBases2 = map snd . frequencies

countBases3 :: String -> [Int]
countBases3 = go 0 0 0 0
  where go a c t g ('A':xs) = go (a + 1) c t g xs
        go a c t g ('C':xs) = go a (c + 1) t g xs
        go a c t g ('G':xs) = go a c (t + 1) g xs
        go a c t g ('T':xs) = go a c t (g + 1) xs
        go a c t g _ = [a, c, t, g]

countBases4 :: String -> [Int]
countBases4 = M.elems . foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

countBases5 :: String -> [Int]
countBases5 str = map (\n -> length $ filter (==n) str) "ACGT"

countBases6 :: String -> [Int]
countBases6 str = map (length . flip filter str . (==)) "ACGT"

countBases7 :: String -> [Int]
countBases7 str = map (($ str) . count) "ACGT"

countBases8 :: String -> [Int]
countBases8 = zipWith ($) (map count "ACGT") . repeat

baseCounters = [countBases0, countBases1, countBases2, countBases3,
                countBases4, countBases5, countBases6, countBases7,
                countBases8]

solveCountBases :: String -> String
solveCountBases = unlines . map (unwords . map show) . zipWith ($) baseCounters . cycle . words

-- Transcribing DNA into RNA
transcribe0 :: String -> String
transcribe0 = replace 'T' 'U'

transcribe1 :: String -> String
transcribe1 [] = []
transcribe1 ('T':xs) = 'U' : transcribe1 xs
transcribe1 (x:xs) = x : transcribe1 xs

transcribe2 :: String -> String
transcribe2 = map pair
  where pair 'T' = 'U'
        pair  x  =  x

transcribe3 :: String -> String
transcribe3 [] = []
transcribe3 (x:xs)
  | x == 'T'  = 'U' : transcribe1 xs
  | otherwise =  x  : transcribe1 xs

transcribers = [transcribe0, transcribe1, transcribe2, transcribe3]

solveTranscribe :: String -> String
solveTranscribe = unlines . zipWith ($) transcribers . cycle . words
-- Dispatch a Solver
main :: IO()
main = interact solveTranscribe
