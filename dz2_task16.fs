// Нахождение максимального простого делителя
let isPrime n =
    let rec check i =
        if i * i > n then true
        elif n % i = 0 then false
        else check (i + 1)
    n > 1 && check 2

let maxPrimeDivisor n =
    let rec findDivisor divisor maxDiv =
        if divisor > n then maxDiv
        elif n % divisor = 0 && isPrime divisor then
            findDivisor (divisor + 1) (max divisor maxDiv)
        else
            findDivisor (divisor + 1) maxDiv
    findDivisor 2 1

let number1 = 60
printfn "Максимальный простой делитель числа %d: %d" number1 (maxPrimeDivisor number1)




// Нахождение произведения цифр числа, не делящихся на 5
let productOfDigitsNotDivisibleBy5 n =
    let rec product digits acc =
        match digits with
        | [] -> acc
        | x :: xs -> 
            if x % 5 <> 0 then
                product xs (acc * x)
            else
                product xs acc
    let digits = n.ToString() |> Seq.map (fun c -> int (string c)) |> Seq.toList
    product digits 1

let number2 = 12345
printfn "Произведение цифр числа %d, не делящихся на 5: %d" number2 (productOfDigitsNotDivisibleBy5 number2)




// Нахождение НОД
let rec gcd a b =
    if b = 0 then a
    else gcd b (a % b)

// Нахождение максимального нечетного непростого делителя числа
let maxOddNonPrimeDivisor n =
    let rec findDivisor divisor maxDiv =
        if divisor > n then maxDiv
        elif n % divisor = 0 && divisor % 2 <> 0 && not (isPrime divisor) then
            findDivisor (divisor + 1) (max divisor maxDiv)
        else
            findDivisor (divisor + 1) maxDiv
    findDivisor 3 1

// Пример использования
let number3 = 60
let prodDigits = productOfDigitsNotDivisibleBy5 number3
let maxOddDiv = maxOddNonPrimeDivisor number3
printfn "НОД максимального нечетного непростого делителя числа %d и произведения цифр: %d" number3 (gcd maxOddDiv prodDigits)