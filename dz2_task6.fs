open System

let chooseFunction condition = 
    match condition with 
    | true -> (fun num ->
        let rec sumOfDigitsRec num currentSum =
            match num with
            | 0 -> currentSum
            | _ -> sumOfDigitsRec (num /10) (currentSum + (num % 10))
        sumOfDigitsRec num 0)
    
    | false -> (fun num ->
        let rec factorialRec num acc = 
            match num with
            | 0 -> acc
            | _ -> factorialRec (num - 1) (num * acc)
        
        factorialRec num 1)

let sumFunction = chooseFunction true
let factorialFunction = chooseFunction false

printfn "Сумма цифр: %d" (sumFunction 1234)
printfn "Факториал: %d" (factorialFunction 5)
