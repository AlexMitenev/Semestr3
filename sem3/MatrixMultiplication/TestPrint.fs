module TestPrint

open System.Diagnostics

let rnd = new System.Random()
let timeCounter = new Stopwatch()

let print m1 m2 threads =
    printfn "%s" "---------matrix1------------"
    printfn "%A" m1

    printfn "%s" "---------matrix2------------"
    printfn "%A" m2

    printfn "%s" "---------result-------------"
    printfn "%A" <| Multyplication.mult m1 m2 threads

let CreateRandomMatrix maxNumber =
    Array2D.init 1000 1000 (fun _ _ -> rnd.Next(maxNumber))

let TestTime m1 m2 threads =
    timeCounter.Start()
    Multyplication.mult m1 m2 threads |> ignore
    timeCounter.Stop()
    printfn "%d threads for matrix mult 1000x1000: %A" threads timeCounter.Elapsed
    timeCounter.Reset()

let Experiment () =
    let m1 = CreateRandomMatrix 5
    let m2 = CreateRandomMatrix 5
    List.iter (TestTime m1 m2) [1..16]


Experiment()



