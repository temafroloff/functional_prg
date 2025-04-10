module JukFunctionalProgramming.Dz1Task3

open System
//������� �����
let calculateCircleArea radius = Math.PI * radius * radius

// ������ ��������
let calculateCylinderVolume radius height = 
    calculateCircleArea radius * height

// ������������
let mainSuperposition () =
    let readInput = Console.ReadLine >> float
    let processData = 
        (fun (r, h) -> calculateCylinderVolume r h) 
        >> (sprintf "����� ��������: %.2f" : float -> string)
        >> Console.WriteLine

    printfn "������� ������:"
    let r = readInput()
    printfn "������� ������:"
    let h = readInput()
    processData (r, h) 

// ������������
let mainCurrying () =
    let readInput() = Console.ReadLine() |> float
    let printResult = 
        (sprintf "����� ��������: %.2f" : float -> string) 
        >> Console.WriteLine

    printfn "������� ������:"
    let radius = readInput()
    printfn "������� ������:"
    let height = readInput()

    calculateCylinderVolume radius height |> printResult


printfn "�������� ����� (1 - ������������, 2 - ������������):"
match Console.ReadLine() with
| "1" -> mainSuperposition()
| "2" -> mainCurrying()
| _ -> printfn "�������� ����"