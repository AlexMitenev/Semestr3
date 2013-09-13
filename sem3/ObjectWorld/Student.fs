open System

type Activity =

    val hardness : int

    new(hard) =
        if hard < 0 && hard > 100 then failwithf "Invalid Hardness"
        {hardness = hard}


type Study =

    inherit Activity

    val needStudSkill : int

    new(hard, skill) = 
        if skill < 0 && skill > 100 then failwithf "Invalid Study Skill"
        {inherit Activity(hard)
         needStudSkill = skill}
    

type Work =

    inherit Activity

    val needWorkSkill : int
    val salary : int

    new(hard, skill, sal) = 
        if skill < 0 then failwithf " Invalid WorkSkill"
        if sal < 0 then failwithf "Salary < 0"
        {inherit Activity(hard)
         needWorkSkill = skill
         salary = sal}
    


type Food =

    val foodCaloric : int
    val foodCost : int

    new(cal, ct) = 
        if cal < 0 then failwithf "Caloric < 0"
        if ct < 0 then failwithf "Cost < 0"
        {foodCaloric = cal;
         foodCost = ct}


type Man =

    val name : string
    val mutable money: int 
    val mutable hungry : int
    val mutable foodList : Food list
    abstract factorSatiety : int
    default this.factorSatiety = 1

    new(name, mon, hun) =
        if name = "" then failwithf "No Name"
        if hun < 0 && hun > 100 then failwithf "Invalid hungry"

        {name = name
         money = mon
         hungry = hun
         foodList = []}

    member this.Eat() =
        if not this.foodList.IsEmpty then this.hungry <- this.hungry + this.foodList.Head.foodCaloric * this.factorSatiety
        this.foodList <- this.foodList.Tail

    member this.BuyFood(fd : Food) =
        this.foodList <- fd :: this.foodList
        this.money <- this.money - fd.foodCost 
            
          
type Student =

    inherit Man

    val mutable studSkill : int
    val mutable studAtUniversity : bool
    override this.factorSatiety = 3

    new(name, mon, hun, lf, ss) =
        if ss < 0 && ss > 100 then failwithf "Invalid Study Skill"
        {inherit Man(name, mon, hun)
         studAtUniversity = true
         studSkill = ss}
        
    member this.GoStudy(subject : Study) =
        if this.studSkill >= subject.needStudSkill && this.studAtUniversity then
           this.studSkill <- this.studSkill + subject.hardness
           this.hungry <- this.hungry - subject.hardness
    
    member public this.ExpelledFromUniver() = 
        this.studAtUniversity <- false
    
      
type Worker =

    inherit Man

    val mutable workSkill : int

    new(name, mon, hun, ws) =
       if ws < 0 && ws > 100 then failwithf "Invalid Work Skill"
       {inherit Man(name, mon, hun)
        workSkill = ws}

    member this.GoWork(workType : Work) =
        if this.workSkill >= workType.needWorkSkill then
           this.workSkill <- this.workSkill + workType.hardness
           this.hungry <- this.hungry + workType.hardness


type Professor =

    inherit Worker

    val anger : int

    new(name, mon, hun, ws, an) =
      if an < 0 && an > 100 then failwithf "Invalid Anger"
      {inherit Worker(name, mon, hun, ws)
       anger = an}
    
    member this.examStudent(stud : Student) =
        if stud.studSkill < this.anger then
           stud.ExpelledFromUniver() |> ignore
           printfn "%s" "student is die=("
            

let hamburher = new Food(10, 300)
let cocacola = new Food(1, 100)
let alex = new Student("alex", 3000, 50, [cocacola; cocacola], 40)
let prepod = new Professor("Jasha", 30000, 10, 80, 50)
let alegbra = new Study(9, 60)
let programming = new Work(5, 30, 5000)
let program = new Study(5, 30)
alex.BuyFood(hamburher)
alex.BuyFood(hamburher)
alex.BuyFood(cocacola)
alex.Eat()
prepod.examStudent(alex)


        
         
        



