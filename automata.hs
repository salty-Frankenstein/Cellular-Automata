import Control.Monad.State

type Grid = [[Int]]

neighbour :: Grid -> (Int, Int) -> Int
neighbour g (i,j) = sum [ g!!x!!y | x <- [i-1..i+1], y <- [j-1..j+1]] - (g!!i!!j)

isAlive :: Grid -> (Int, Int) -> Int
isAlive g (i,j) = 
    case neighbour g (i,j) of
        3 -> 1
        2 -> g!!i!!j
        _ -> 0

getNext :: Grid -> Grid
getNext g = let i = length g; j = length $ head g in
    [
        [   if x == 0 || y == 0 || x == i-1 || y == j-1 
            then 0
            else isAlive g (x,y) 
            | y <- [0..j-1] 
        ] 
        | x <- [0..i-1] 
    ]

nextState :: State Grid Grid
nextState = state $ \g -> (g, getNext g)


test :: Grid
test = let zero = take 8 $ repeat 0 in 
    [zero] ++ map (\x->[0]++x++[0]) [[mod (x*y+x) 2 | y <- [1..6]] | x <- [1..5]] ++ [zero]

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

main :: IO ()
main = do
    s <- return nextState
    printList $ fst $ runState s test
    s1 <- return (s >>= \_->nextState)
    printList $ fst $ runState s1 test
    getLine
    return ()
