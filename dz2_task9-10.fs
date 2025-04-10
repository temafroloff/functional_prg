let processDigitsWithCondition number operation initial condition =
    let rec loop num acc =
        match num with
        | 0 -> acc
        | _ ->
            match condition (num % 10) with
            | true -> loop (num / 10) (operation (num % 10) acc)
            | false -> loop (num / 10) acc
    
    loop number initial

printfn "Сумма чётных цифр: %d" (processDigitsWithCondition 7112 (fun x y -> x + y) 0 (fun x -> x % 2 = 0))
printfn "Произведение нечётных цифр: %d" (processDigitsWithCondition 2713 (fun x y -> x * y) 1 (fun x -> x % 2 <> 0))
printfn "Максимальная чётная цифра: %d" (processDigitsWithCondition 71128 max 0 (fun x -> x % 2 = 0))
printfn "Минимальная нечётная цифра: %d" (processDigitsWithCondition 71139 min 9 (fun x -> x % 2 <> 0))