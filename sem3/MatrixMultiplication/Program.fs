// Asinc Matrix Multiplication
//Alex Mitenev
//271 group 

let rnd = new System.Random()

let mult (a:int[,]) (b:int[,]) =
    let stringsA = Array2D.length1 a
    let columnsA = Array2D.length2 a
    let stringsB = Array2D.length1 b
    let columnsB = Array2D.length2 b

    let result = Array2D.create stringsA columnsB 0

    [|for i in 0 .. stringsA - 1 ->
          async{
              for j in 0 .. columnsB - 1 do
                 for k in 0 .. columnsA - 1 do
                     result.[i,j] <- result.[i,j] + a.[i,k] * b.[k,j]
          }|]

    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    result


let printMult m1 m2 =
    printfn "%s" "---------matrix1------------"
    printfn "%A" m1

    printfn "%s" "---------matrix2------------"
    printfn "%A" m2

    printfn "%s" "---------result-------------"
    printfn "%A" <|  mult m1 m2


let m1 = Array2D.init 1000 1000 (fun _ _ -> rnd.Next(5))
let m2 = Array2D.init 1000 1000 (fun _ _ -> rnd.Next(5))

printMult m1 m2