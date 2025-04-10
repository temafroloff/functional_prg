let processFilteredCoprimeDigits number operation initialValue condition =
    let rec process remainingNumber result =
        match remainingNumber with
        | 0 -> result
        | _ ->
            let digit, nextNumber = remainingNumber % 10, remainingNumber / 10
            let updatedResult =
                match isCoprime digit number && condition digit with
                | true -> operation result digit
                | false -> result
            process nextNumber updatedResult

    process number initialValue

printfn "Сумма цифр, взаимно простых с 365 и > 3: %d" 
    (processFilteredCoprimeDigits 365 (+) 0 (fun x -> x > 3))

printfn "Произведение цифр, взаимно простых с 271828 и > 1: %d" 
    (processFilteredCoprimeDigits 271828 (*) 1 (fun x -> x > 1))