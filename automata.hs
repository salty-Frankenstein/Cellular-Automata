import Control.Monad.State
import System.Process

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

nextStateIO :: StateT Grid IO Grid
nextStateIO = StateT $ \g -> return (g, getNext g)

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs

run :: StateT Grid IO ()
run = do
    liftIO $ system "cls"
    s <- nextStateIO
    liftIO $ printList s
    liftIO getLine
    return ()

initial :: Grid
initial = let zero = take 8 $ repeat 0 in 
    [zero] ++ map (\x->[0]++x++[0]) [[mod (x*y+x) 2 | y <- [1..6]] | x <- [1..5]] ++ [zero]

bindStream :: StateT Grid IO () -> ((), Grid) -> IO ((), Grid)
bindStream run = \(_,l) -> runStateT run l >>= bindStream run

foreverDo :: StateT Grid IO () -> IO ((), Grid)
foreverDo run = runStateT run initial >>= bindStream run

main :: IO ()
main = do
    foreverDo run
    return ()
