open System
open NUnit.Framework
open FsUnit
open System.Text.RegularExpressions

type MailCheck() =
    let first = "^[-a-zA-Z_]"
    let siteName = "([\.]?[-a-zA-Z0-9!#$%&'*+/=?^_`{|}~]+)*@"
    let beforeAt = first + siteName
    let afterAt = "[-0-9A-Za-z_]{1,64}\.[a-zA-Z]{2,6}$"
    let sum = beforeAt + afterAt
    let reg = new Regex(sum)
    member public this.Check mail =
        reg.IsMatch mail


[<TestFixture>] 
type ``Good tests`` ()=
    let mc = new MailCheck()

    [<Test>] member x.
     ``short email`` ()=
            mc.Check "a@b.cc" |> should be True

    [<Test>] member x.
     ``enother standart email`` ()=
            mc.Check "victor.polozov@mail.ru" |> should be True

    [<Test>] member x.
     ``.info domen`` ()=
            mc.Check "my@domain.info" |> should be True

    [<Test>] member x.
     ``simbol domen`` ()=
            mc.Check "_.1@mail.com" |> should be True
  
    [<Test>] member x.
     ``long domain`` ()=
            mc.Check "coins_department@hermitage.museum" |> should be True

[<TestFixture>] 
type ``Bad emails`` ()=
    
    let mc = new MailCheck()

    [<Test>] member x.
     ``1 char in domain top level`` ()=
            mc.Check "a@b.c" |> should be False

    [<Test>] member x.
     ``double dot domain`` ()=
            mc.Check "a..b@mail.ru" |> should be False

    [<Test>] member x.
     ``domain without body`` ()=
            mc.Check ".a@mail.ru" |> should be False

    [<Test>] member x.
     ``long domain top level`` ()=
            mc.Check "yo@domain.somedomain" |> should be False

    [<Test>] member x.
     ``only one num in body`` ()=
            mc.Check "1@mail.ru" |> should be False