import Control.Monad

main = putStrLn $ foldl (++) "" (map show solution)

solution :: [Int]
solution = do
    let digit = [0..9]
    a <- digit
    b <- digit
    c <- digit
    d <- digit
    guard $ a/=b
    guard $ a/=c
    guard $ a/=d
    guard $ b/=c
    guard $ b/=d
    guard $ (1000*a+100*b+10*c+d)*d == 1000*d+100*c+10*b+a
    [a,b,c,d]
