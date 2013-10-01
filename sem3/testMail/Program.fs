open System
open NUnit.Framework
open FsUnit
open System.Text.RegularExpressions

let mailTest expr = 
    let beforeAt = "^[0-9a-zA-Z!#$%&'*+-/=?^_`{|}~]{2,64}@"
    let afterAt = "[-0-9A-Za-z_^\.]{2,192}\.[a-zA-Z]{2,6}$"
    let sum = beforeAt + afterAt
    let reg = new Regex(sum)
    reg.IsMatch(expr)

let test string =
    if not (mailTest string) then 
        printf "%s" "error in "
        printfn "%s" string

[<TestFixture>] 
type ``Good tests`` ()=

    [<Test>] member x.
     ``standart email`` ()=
            mailTest "AlexMitenev@gmail.com" |> should be True

    [<Test>] member x.
     ``enother standart email`` ()=
            mailTest "VvV@gmail.com" |> should be True

    [<Test>] member x.
     ``email with 2 domain`` ()=
            mailTest "lolo.ayayay.lolo@lolo.lolo" |> should be True

[<TestFixture>] 
type ``Bad emails`` ()=

    [<Test>] member x.
     ``1 char in domain top level`` ()=
            mailTest "AlexMitenev@gmail.c" |> should be False

    [<Test>] member x.
     ``colon before domain top level`` ()=
            mailTest "AlexMitenev@gmail:com" |> should be False

    [<Test>] member x.
     ``email without domain`` ()=
            mailTest "AlexMitenev@gmail" |> should be False

    [<Test>] member x.
     ``short email`` ()=
            mailTest "A@t.ysdf" |> should be False