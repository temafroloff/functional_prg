let rec foldDigits num func acc =
    match num with
    | 0 -> acc
    | _ -> foldDigits (num / 10) func (func acc (num % 10))

let processDigits numbers operetions initial = 
    let rec loop num acc = 
        match num with
        | 0 -> acc
        | _ ->
            let digit = num % 10
            loop (num / 10) (operetions digit acc)
    loop numbers initial

printfn "Сумма цифр: %d" (processDigits 711 (fun x y -> x + y) 0)
printfn "Произведение цифр: %d" (processDigits 271 (fun x y -> x * y) 1)
printfn "Максимальная цифра: %d" (processDigits 711 max 0)
printfn "Минимальная цифра: %d" (processDigits 711 min 9)