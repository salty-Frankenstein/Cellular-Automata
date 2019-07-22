// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Threading
open System.Windows.Forms
open System.Drawing

let readInt()=int (Console.ReadLine())
let pause()=Console.ReadLine()

type Matrix = int [,]   //Matrix类型为int二维数组

let PrintMatrix (m:Matrix) =     
    let length0 = int(m.GetLongLength 0)
    let length1 = int(m.GetLongLength 1)
    for i = 0 to length0-1 do
        for j = 0 to length1-1 do
            printf "%d" m.[i,j]
        printf "\n"


let PrintMachine (m:Matrix) =       //输出一个元胞自动机状态
    let length0 = int(m.GetLongLength 0)
    let length1 = int(m.GetLongLength 1)
    for i = 0 to length0-1 do
        for j = 0 to length1-1 do
            if m.[i,j] = 0 then printf " "
            else printf "#"
        printf "\n"


let GetMatrix (x,y) (m:Matrix) =    //(int,int) -> Matrix -> Matrix 从矩阵m中取以(x,y)为中心的3*3子矩阵，不足截去
    let length0 = int(m.GetLongLength 0)
    let length1 = int(m.GetLongLength 0)
    let up = max (x-1) 0
    let down = min (x+1) (length0-1)
    let left = max (y-1) 0
    let right = min (y+1) (length1-1)
    m.[up..down,left..right]


let GetMatrix3 (x,y) (m:Matrix) =   //(int,int) -> Matrix -> Matrix 从矩阵m中取以(x,y)为中心的3*3子矩阵，不足补0
    let length0 = int(m.GetLongLength 0)
    let length1 = int(m.GetLongLength 1)
    let n = Array2D.create (length0+2) (length1+2) 0
    for i = 0 to length0-1 do
        for j = 0 to length1-1 do
            n.[i+1,j+1] <- m.[i,j]
    n.[x..x+2,y..y+2]
    

let GetNext (m:Matrix) =  //Matrix -> int 由一个3*3子矩阵得到中心元素的下一个状态
    let mutable sum = 0;
    for i = 0 to 2 do 
        for j = 0 to 2 do
            sum <- sum + m.[i,j]
    sum <- sum - m.[1,1]    //减去中间元素，得到周围元素之和
    match sum with          //匹配元胞自动机状态
    | 3 -> 1
    | 2 -> m.[1,1]
    | _ -> 0


let NextState (m:Matrix) = // Matrix -> Matrix 计算矩阵的下一个状态
    let length0 = int(m.GetLongLength 0)
    let length1 = int(m.GetLongLength 1)
    let n = Array2D.create (length0) (length1) 0
    
    for i = 0 to length0-1 do
        for j = 0 to length1-1 do
            n.[i,j] <- m |> GetMatrix3 (i,j) |> GetNext
    n


let CreateNum (r:Random) x y  = //Random -> int -> int -> int 随机生成布尔量
    r.Next 2

let CreateMatrix (r:Random) (x,y) = //Random -> (int, int) -> Matrix 随机生成x*y矩阵
    Array2D.init x y (CreateNum r)


let testMatrix = array2D [ [ 1; 0; 1; 0];
                           [ 0; 1; 0; 1];
                           [ 1; 1; 1; 1];
                           [ 0; 0; 0; 0]]

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
 