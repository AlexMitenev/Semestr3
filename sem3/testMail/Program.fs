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

test "AlexMitenev@gmail.com"
test "AlexMitenev@gmail.c"
test "AlexMitenev@gmail"
test "AlexMitenev"
test "AlexMitenev@gmail:com"
test "lolo.ayayay.lolo@lolo.lolo"
test "A@t.ysdf"
test "AlexMitenev@gmail.comsdfsdfsdf"
test "!!!@gmail.cot"
test "l!$l@gmail.com"
test "VvV@gmail.com"
test "AlexMitenev@gmail.lolo.rere.lala.com"
