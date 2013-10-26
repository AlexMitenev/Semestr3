open System
open NUnit.Framework
open FsUnit
open System.Text.RegularExpressions

let mailTest expr = 
    let beforeAt = "^[-a-z0-9!#$%&'*+/=?^_`{|}~]+(\.[-a-z0-9!#$%&'*+/=?^_`{|}~]+)*@"
    let afterAt = "[-0-9A-Za-z_^\.]{1,192}\.[a-zA-Z]{2,6}$"
    let doubleDot = new Regex("\.\.")
    let sum = beforeAt + afterAt
    let reg = new Regex(sum)
    reg.IsMatch(expr) &&  not <| doubleDot.IsMatch(expr)

[<TestFixture>] 
type ``Good tests`` ()=

    [<Test>] member x.
     ``short email`` ()=
            mailTest "a@b.cc" |> should be True

    [<Test>] member x.
     ``enother standart email`` ()=
            mailTest "victor.polozov@mail.ru" |> should be True

    [<Test>] member x.
     ``.info domen`` ()=
            mailTest "my@domain.info" |> should be True

    [<Test>] member x.
     ``simbol domen`` ()=
            mailTest "_.1@mail.com" |> should be True
  
    [<Test>] member x.
     ``long domain`` ()=
            mailTest "coins_department@hermitage.museum" |> should be True

[<TestFixture>] 
type ``Bad emails`` ()=

    [<Test>] member x.
     ``1 char in domain top level`` ()=
            mailTest "a@b.c" |> should be False

    [<Test>] member x.
     ``double dot domain`` ()=
            mailTest "a..b@mail.ru" |> should be False

    [<Test>] member x.
     ``domain without body`` ()=
            mailTest ".a@mail.ru" |> should be False

    [<Test>] member x.
     ``long domain top level`` ()=
            mailTest "yo@domain.somedomain" |> should be False

    [<Test>] member x.
     ``only one num in body`` ()=
            mailTest "1@mail.ru" |> should be False