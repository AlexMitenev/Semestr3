open System
 
type Product() = 
    let mutable parts = []
    member x.Parts () = parts
    member x.Add s = parts <- s :: parts
    member x.Show() = printfn "%s" "Product Parts:"
                      List.map (printfn "%s") parts

[<AbstractClass>]
type Builder() = 
    let product = new Product()
    member x.Product = product
    abstract member BuildPartA : unit -> unit
    abstract member BuildPartB : unit -> unit
    abstract member GetResult : unit -> Product
     
type ConcreteBuilder1() =
    inherit Builder()   
    override x.BuildPartA() = x.Product.Add "PartA"
    override x.BuildPartB() = x.Product.Add "PartB"
    override x.GetResult() = x.Product
 
type ConcreteBuilder2()  =
    inherit Builder()
    override x.BuildPartA() = x.Product.Add "PartX"
    override x.BuildPartB() = x.Product.Add "PartY"
    override x.GetResult() = x.Product

     
type Director() =
    member x.Construct (builder:Builder) = 
        builder.BuildPartA()
        builder.BuildPartB()
 
// Create director and builders
let director = new Director()
 
let b1 = new ConcreteBuilder1()
let b2 = new ConcreteBuilder2()
 
// Construct two products
director.Construct(b1)
let res1 = b1.GetResult()
res1.Show() |> ignore
 
director.Construct(b2)
let res2 = b2.GetResult()
res2.Show() |> ignore