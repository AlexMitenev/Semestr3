open System
open NUnit.Framework
open FsUnit

type Activity(name, hardness) =
    do if hardness < 0 && hardness > 100 then 
        failwithf "Invalid Hardness: %d is not in 0..100 range" hardness
    member x.Hardness = hardness
    member x.Name = name

type Study(name, hardness, needStudSkill) =
    inherit Activity(name, hardness)
    do if needStudSkill < 0 && needStudSkill > 100 then 
          failwithf "Invalid NeedStudSkill: %d is not in 0..100 range" needStudSkill
    member x.NeedStudSkill = needStudSkill
    
type Work(name, hardness, needWorkSkill, salary) =
    inherit Activity(name, hardness)
    do if needWorkSkill < 0 && needWorkSkill > 100 then 
          failwithf "Invalid needWorkSkill: %d is not in 0..100 range" needWorkSkill
       if salary < 3000 && salary > 1000000 then 
          failwithf "Invalid Salary: %d is not in 0..100 range" salary
    member x.NeedWorkSkill = needWorkSkill
    member x.Salary = salary     
    
type Food(name, foodCaloric, foodCost) =
    do if foodCaloric < 0 && foodCaloric > 100 then 
          failwithf "Invalid foodCaloric: %d is not in 0..100 range" foodCaloric
       if foodCost < 3000 && foodCost > 1000000 then 
          failwithf "Invalid Salary: %d is not in 0..100 range" foodCost
    member x.Name = name
    member x.FoodCaloric = foodCaloric
    member x.FoodCost = foodCost

type Man =
    val name : string
    val mutable money: int 
    val mutable hungry : int
    val mutable foodList : Food list
    abstract factorSatiety : int
    default this.factorSatiety = 1
    new(name, mon, hun) =
        if name = "" then failwithf "No Name"
        if hun < 0 && hun > 100 then failwithf "Invalid Hungry: %d is not in 0..100 range" hun
        {name = name
         money = mon
         hungry = hun
         foodList = []}

    member this.Eat() =
        if not this.foodList.IsEmpty then this.hungry <- this.hungry + this.foodList.Head.FoodCaloric * this.factorSatiety
        this.foodList <- this.foodList.Tail

    member this.BuyFood(fd : Food) =
        this.foodList <- fd :: this.foodList
        this.money <- this.money - fd.FoodCost
            
          
type Student =
    inherit Man
    val mutable studSkill : int
    val mutable studAtUniversity : bool
    override this.factorSatiety = 3

    new(name, mon, hun, lf, ss) =
        if ss < 0 && ss > 100 then failwithf "Invalid Study Skill: %d is not in 0..100 range" ss
        {inherit Man(name, mon, hun)
         studAtUniversity = true
         studSkill = ss}
        
    member this.GoStudy(subject : Study) =
        if this.studSkill >= subject.NeedStudSkill && this.studAtUniversity 
        then
           this.studSkill <- this.studSkill + subject.Hardness
           this.hungry <- this.hungry - subject.Hardness
        else 
           printfn "%s is too hard" subject.Name
    
    member public this.ExpellFromUniver() = 
        this.studAtUniversity <- false
    
      
type Worker =
    inherit Man
    val mutable workSkill : int

    new(name, mon, hun, ws) =
       if ws < 0 && ws > 100 then failwithf "Invalid Work Skill: %d is not in 0..100 range" ws
       {inherit Man(name, mon, hun)
        workSkill = ws}

    member this.GoWork(workType : Work) =
        if this.workSkill >= workType.NeedWorkSkill then
           this.workSkill <- this.workSkill + workType.Hardness
           this.hungry <- this.hungry + workType.Hardness


type Professor =
    inherit Worker
    val anger : int

    new(name, mon, hun, ws, an) =
      if an < 0 && an > 100 then failwithf "Invalid Anger: %d is not in 0..100 range" an
      {inherit Worker(name, mon, hun, ws)
       anger = an}
           
    member this.examStudent(stud : Student) =
        if stud.studSkill < this.anger then
           stud.ExpellFromUniver() |> ignore

[<Test>] 
let GoodScenary() =          
    let hamburher = new Food("hamburher", 10, 300)
    let cocacola = new Food("cocacola", 1, 100)
    let alex = new Student("Alex", 3000, 50, [cocacola; cocacola], 50)
    let prepod = new Professor("Victor Sergeevich", 30000, 10, 80, 60)
    let program = new Study("program", 5, 30)
    alex.BuyFood(hamburher)
    alex.Eat()
    alex.GoStudy(program)
    alex.GoStudy(program)
    alex.GoStudy(program)
    alex.GoStudy(program)
    prepod.examStudent(alex)
    alex.studAtUniversity |> should be True 

[<Test>] 
let BadScenary() =          
    let hamburher = new Food("hamburher", 10, 300)
    let cocacola = new Food("cocacola", 1, 100)
    let kirill = new Student("Kirill", 3000, 50, [cocacola; cocacola], 40)
    let prepod = new Professor("Ivanov", 30000, 10, 80, 70)
    let matan = new Study("matan", 10, 60)
    kirill.BuyFood(hamburher)
    kirill.BuyFood(hamburher)
    kirill.BuyFood(cocacola)
    kirill.Eat()
    kirill.GoStudy(matan)
    kirill.GoStudy(matan)
    prepod.examStudent(kirill)
    kirill.studAtUniversity |> should be False