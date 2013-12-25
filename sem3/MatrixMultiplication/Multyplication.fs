// Asinc Matrix Multiplication
//Alex Mitenev
//271 group 

module Multyplication

let mult (a:int[,]) (b:int[,]) (th : int) =
    let stringsA = Array2D.length1 a
    let columnsA = Array2D.length2 a
    let stringsB = Array2D.length1 b
    let columnsB = Array2D.length2 b
    let result = Array2D.create stringsA columnsB 0
        
    let oneAsyncMul l = 
        [|for i in l ->
              async{
                  for j in 0 .. columnsB - 1 do
                     for k in 0 .. columnsA - 1 do
                         result.[i, j] <- result.[i, j] + a.[i, k] * b.[k, j]
              }|]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore

    // make list of lists for process who run parallel 
    let makeList (l : int []) ths=
        let len = l.Length
        [for i in 0..ths..len - 1 -> if (i < ths * (len / ths)) 
                                     then l.[i..i + ths - 1] 
                                     else l.[i..len - 1]] 

    let list = makeList [|0..stringsA - 1|] th

    List.iter oneAsyncMul list

    result