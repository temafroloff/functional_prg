let rec gcd a b = 
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)

let isCoprime a b =
    match gcd a b with
    | 1 -> true
    | _ -> false

let processCoprimeDigits number operation init =
    let rec step num acc =
        match num with
        | 0 -> acc
        | _ -> 
            let digit = num % 10
            let nextNum = num / 10
            let newAcc = 
                match isCoprime digit number with
                | true -> operation acc digit
                | false -> acc
            step nextNum newAcc

    step number init

let eulerTotient n =
    let rec countCoprimes i acc =
        match i with
        | 0 -> acc
        | _ ->
            let newAcc = 
                match isCoprime i n with
                | true -> acc + 1
                | false -> acc
            countCoprimes (i - 1) newAcc

    countCoprimes n 0

System.Console.WriteLine(processCoprimeDigits 365 (+) 0)
System.Console.WriteLine(processCoprimeDigits 271828 ( * ) 1) 
System.Console.WriteLine(processCoprimeDigits 271828 min 9)
System.Console.WriteLine(processCoprimeDigits 271828 max 0)

System.Console.WriteLine(eulerTotient 9)
System.Console.WriteLine(eulerTotient 10)
System.Console.WriteLine(eulerTotient 15)