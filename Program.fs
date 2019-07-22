// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Threading
open System.Windows.Forms
open System.Drawing

let readInt()=int (Console.ReadLine())
let pause()=Console.ReadLine()

type Elem = int*int         //定义元素Elem类型为二元组(现在状态,周围之和)
type Matrix = Elem [,]      //Matrix类型为Elem二维数组


let PrintMatrix (m:Matrix) x =     
    let length0 = int(m.GetLongLength 0)
    let length1 = int(m.GetLongLength 1)
    for i = 0 to length0-1 do
        for j = 0 to length1-1 do
            match x with
            |0 -> printf "%d" (fst m.[i,j])
            |_ -> printf "%d" (snd m.[i,j])
        printf "\n"


let PrintMachine (m:Matrix) =       //输出一个元胞自动机状态
    let length0 = int(m.GetLongLength 0)
    let length1 = int(m.GetLongLength 1)
    for i = 0 to length0-1 do
        for j = 0 to length1-1 do
            match fst m.[i,j] with
            |0 -> printf " "
            |_ -> printf "#"
        printf "\n"


let GetMatrix3 (x,y) (m:Matrix) =   //(int,int) -> Matrix -> Matrix 从矩阵m中取以(x,y)为中心的3*3子矩阵，不足补0
    let length0 = int(m.GetLongLength 0)
    let length1 = int(m.GetLongLength 1)
    let n = Array2D.create (length0+2) (length1+2) (0,0)
    for i = 0 to length0-1 do
        for j = 0 to length1-1 do
            n.[i+1,j+1] <- m.[i,j]
    n.[x..x+2,y..y+2]

    
let GetValue (m:Matrix) = //Matrix -> Elem 从三阶矩阵得到和
    let mutable sum = 0
    for i = 0 to 2 do
        for j = 0 to 2 do
            sum <- sum + fst m.[i,j]
    (fst m.[1,1] , sum - fst m.[1,1])


let InitMatrix (m:Matrix) = //初始化周围和信息，需重写
    let length0 = int(m.GetLongLength 0)
    let length1 = int(m.GetLongLength 1)
    let n = Array2D.create (length0) (length1) (0,0)
    for i = 0 to length0-1 do
        for j = 0 to length1-1 do
            n.[i,j] <- m |> GetMatrix3 (i, j) |> GetValue
    n


let GetNext ori =  //Elem -> Elem 由一个周围数之和得到中心元素的下一个状态
    match ori with          //匹配元胞自动机状态
    | (_, 3) -> (1, 0)
    | (_, 2) -> ori
    | _ -> (0, 0)


let NextState (m:Matrix) = 
    m |> InitMatrix |> Array2D.map GetNext


let CreateNum (r:Random) x y  = //Random -> int -> int -> Elem 随机生成fst为布尔量的Elem
    (r.Next 2, 0)

let CreateMatrix (r:Random) (x,y) = //Random -> (int, int) -> Matrix 随机生成x*y矩阵
    Array2D.init x y (CreateNum r)


[<EntryPoint>]
let main argv = 

    let rand = Random()
    
    printfn "size of the Matrix? Input x,y"
    let x = readInt()
    let y = readInt()

    let mutable tm = (CreateMatrix rand (x,y))

    PrintMachine tm

    printfn "iter for _ times?"
    let a = readInt()
    Console.Clear()
    
    for i = 1 to a do
        tm <- tm |> NextState
        tm |> PrintMachine
        printfn "%d" i
        Thread.Sleep(200)
        Console.Clear()
    
    tm |> PrintMachine
    printf "over\n" 
    pause() |> ignore
    
    0 // return an integer exit code
 