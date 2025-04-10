module JukFunctionalProgramming.Dz1Task

open System
// �������� ����� 
let rec sumDigitsUp n =
    let absN = abs n
    if absN = 0 then 0
    else (absN % 10) + sumDigitsUp (absN / 10)


// ��������� �������� 
let sumDigitsTail n =
    let rec helper acc num =
        let absNum = abs num
        if absNum = 0 then acc
        else helper (acc + absNum % 10) (absNum / 10)
    helper 0 n

let rec countDivisors n divisor =
    match divisor with
    | d when d > n -> 0
    | d when d % n = 0 -> 
        1 + countDivisors n (d + 1)
    | _ -> countDivisors n (divisor + 1)

let countDivisorsOf n =
    countDivisors n 1



let readNumber() =
    printfn "������� ����� �����:"
    match Console.ReadLine() |> Int32.TryParse with
    | (true, num) -> num
    | _ -> 
        printfn "������������ ����"
        0


let main() =
    let number = readNumber()
    printfn "�������� �����: %d" (sumDigitsUp number)
    printfn "��������� ��������: %d" (sumDigitsTail number)
    printfn "����� ���������: %d" (countDivisorsOf number)

main()