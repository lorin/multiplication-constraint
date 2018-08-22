import Control.Monad
import Data.Set (size, fromList)

main = putStrLn $ foldl (++) "" (map show solution)

-- Check if all elements of a list are differnet
different :: Ord a => [a] -> Bool
different l = length l == (size . fromList) l

solution :: [Int]
solution = do
    let digit = [0..9]
    a <- digit
    b <- digit
    c <- digit
    d <- digit
    guard $ different [a,b,c,d]
    guard $ (1000*a+100*b+10*c+d)*d == 1000*d+100*c+10*b+a
    [a,b,c,d]
