import Data.List (unfoldr, tails)
import System.Random (newStdGen, StdGen(..), random)

-- n last elements of array
nLast :: Int -> [a] -> [a]
nLast n xs = foldl (const . tail) xs (drop n xs)

candidates :: Eq a => [a] -> [a] -> [a]
candidates seed window = (map head -- get the first element after last element in `window`
  . filter (not . null) -- filter empty array
    . map (drop wl) -- drop the the part that is already in `window`
      . filter ((==) window . take wl) -- filter for suffixes starting with `window`
        . tails) seed -- create suffix array of seed
  where wl = length window

generate :: Eq a => [a] -> [a] -> StdGen -> [[a]]
generate seed start g = map snd $ unfoldr lam (g, start)
  where lam (g', curr) = Just ((g', curr), (gNext g', curr ++ [randomElem (candidates seed (win curr)) (r g')]))
        win        = nLast 5
        r          = fst . (random :: StdGen -> (Int, StdGen))
        gNext      = snd . (random :: StdGen -> (Int, StdGen))
        randomElem arr n = arr !! mod n (length arr)

main = do
  seed <- getContents
  g <- newStdGen 
  let start = take 5 seed
  putStrLn $ last . take 5000 $ generate seed start g
