open System
open NUnit.Framework
//open FsUnit

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
          failwithf "Invalid Salary: %d is not in 3000..1000000 range" salary
    member x.NeedWorkSkill = needWorkSkill
    member x.Salary = salary     
    

type Food(name, foodCaloric, foodCost) =
    do 
       if foodCaloric < 0 && foodCaloric > 100 then 
          failwithf "Invalid foodCaloric: %d is not in 0..100 range" foodCaloric
       if foodCost < 30 && foodCost > 5000 then 
          failwithf "Invalid Salary: %d is not in 30..5000 range" foodCost
    member x.Name = name
    member x.FoodCaloric = foodCaloric
    member x.FoodCost = foodCost


[<AbstractClass>]
type Man( name : string, money: int, hungry : int, foodList : Food list ) =
    let name             = name
    let mutable  money   = money
    let mutable hungry   = hungry
    let mutable foodList = foodList
    do
        if name = "" then failwithf "No Name"
        if hungry < 0 && hungry > 100 then 
            failwithf "Invalid Hungry: %d is not in 0..100 range" hungry

    member this.Hungry
        with get()      = hungry
        and  set(value) = hungry <- value

    member this.Eat() =
        if   not foodList.IsEmpty 
        then hungry <- hungry + foodList.Head.FoodCaloric
        foodList    <- foodList.Tail

    member this.BuyFood(fd : Food) =
        foodList <- fd :: foodList
        money    <- money - fd.FoodCost       
     
         
type Student( name : string, money: int, hungry : int, foodList : Food list, studySkill : int ) =
    inherit Man( name, money, hungry, foodList )

    let mutable studySkill = studySkill
    let mutable studyAtUniversity = true
    
    member this.StudySkill = studySkill

    member this.StudyAtUniversity 
        with get() = studyAtUniversity
        and set(value) = studyAtUniversity <- value
       
    member this.GoStudy( subject : Study ) =
        if studySkill >= subject.NeedStudSkill && studyAtUniversity 
        then
           studySkill  <- studySkill + subject.Hardness
           this.Hungry <- hungry - subject.Hardness
        else 
           printfn "%s is too hard" subject.Name
    
    member public this.ExpellFromUniver() = 
        this.StudyAtUniversity <- false
    
[<AbstractClass>]      
type Worker ( name : string, money: int, hungry : int, foodList : Food list, workSkill : int ) =
    inherit Man ( name, money, hungry, foodList)
    let mutable workSkill = workSkill

    member this.GoWork(workType : Work) =
        if workSkill >= workType.NeedWorkSkill 
        then
           workSkill   <- workSkill + workType.Hardness
           this.Hungry <- hungry - workType.Hardness


type Professor ( name : string, money: int, hungry : int, foodList : Food list, workSkill : int, anger: int ) =
    inherit Worker ( name, money, hungry, foodList, workSkill )
    let anger = anger
           
    member this.examStudent(stud : Student) =
        if   stud.StudySkill < anger 
        then stud.ExpellFromUniver() |> ignore

[<Test>] 
let GoodScenary() =          
    let hamburher = new Food("hamburher", 10, 300)
    let cocacola = new Food("cocacola", 1, 100)
    let alex = new Student("Alex", 3000, 50, [cocacola; cocacola], 50)
    let professor = new Professor("Victor Sergeevich", 30000, 10, [cocacola; cocacola], 80, 60)
    let program = new Study("program", 5, 30)
    alex.BuyFood(hamburher)
    alex.Eat()
    alex.GoStudy(program)
    alex.GoStudy(program)
    alex.GoStudy(program)
    alex.GoStudy(program)
    professor.examStudent(alex)
    alex.studAtUniversity |> should be True 


[<Test>] 
let BadScenary() =          
    let hamburher = new Food("hamburher", 10, 300)
    let cocacola = new Food("cocacola", 1, 100)
    let kirill = new Student("Kirill", 3000, 50, [cocacola; cocacola], 40)
    let professor = new Professor("Ivanov", 30000, 10, [cocacola; cocacola], 80, 70)
    let matan = new Study("matan", 10, 60)
    kirill.BuyFood(hamburher)
    kirill.BuyFood(hamburher)
    kirill.BuyFood(cocacola)
    kirill.Eat()
    kirill.GoStudy(matan)
    kirill.GoStudy(matan)
    professor.examStudent(kirill)
    kirill.studAtUniversity |> should be False