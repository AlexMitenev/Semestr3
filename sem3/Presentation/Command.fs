open System
open System.Collections

type Calculator() =
    let mutable current = 0 //default start value

    member this.Operation(operator : char, operand : int) =
        match operator with
            |'+' ->  current <- current + operand
            |'-' ->  current <- current - operand
            |'*' ->  current <- current * operand
            |'/' ->  current <- current / operand
            | _  ->  current <- current
        printfn "Current value = %d (following %c %d)" current operator operand


type Command =
    abstract member Execute   : unit -> unit
    abstract member UnExecute : unit -> unit


type CalculatorCommand(operator : char, operand : int, calculator : Calculator) =
    let mutable operator = operator
    let mutable operand = operand
    let mutable calculator = calculator
    
    interface Command with
        override this.Execute()   = calculator.Operation(operator, operand)
        override this.UnExecute() = calculator.Operation(this.Undo(operator), operand)

    member public this.Operator(value)   = operator   <- value
 
    member public this.Operand(value)    = operand    <- value  

    member public this.Calculator(value) = calculator <- value    
 
    member this.Undo(operator : char) =
        match operator with
        | '+' -> '-'
        | '-' -> '+'
        | '*' -> '/'
        | '/' -> '*'
        | _   -> ' '

type User() =
    let calculator = new Calculator()
    let mutable commands  = new ResizeArray<CalculatorCommand>() 
    
    let mutable current = 0

    member public this.Redo(levels : int) =
        printfn "\n---- Redo %d levels " levels
        for i in [0..levels - 1] do
            if current < commands.Count then
                (commands.[current] :> Command).Execute()
                current <- current + 1
                
    member public this.Undo(levels : int) =
        printfn "\n---- Undo %d levels " levels 
        for i in [0..levels - 1] do
            if current > 0 then
                current <- current - 1
                (commands.[current] :> Command).UnExecute()
                
    member public this.Compute(operator : char, operand : int) =
        //Создаем команду операции и выполняем её
        let command = new CalculatorCommand(operator, operand, calculator)
        (command :> Command).Execute()
 
        //Добавляем операцию к списку отмены
        commands.Add(command)
        current <- current + 1


let run =

      let user = new User()

      user.Compute('+', 100)
      user.Compute('-', 50)
      user.Compute('*', 10)
      user.Compute('/', 2)

      user.Undo(3)

      user.Redo(2)