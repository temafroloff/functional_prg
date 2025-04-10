open System


(*
let calculateCircleArea radius = Math.PI * radius * radius

let calculateCylinderVolume radius height = 
    calculateCircleArea radius * height

let mainSuperposition () =
    let readInput = Console.ReadLine >> float
    let processData = 
        (fun (r, h) -> calculateCylinderVolume r h) 
        >> (sprintf "Объем цилиндра: %.2f" : float -> string)
        >> Console.WriteLine

    printfn "Введите радиус:"
    let r = readInput()
    printfn "Введите высоту:"
    let h = readInput()
    processData (r, h) 

let mainCurrying () =
    let readInput() = Console.ReadLine() |> float
    let printResult = 
        (sprintf "Объем цилиндра: %.2f" : float -> string) 
        >> Console.WriteLine

    printfn "Введите радиус:"
    let radius = readInput()
    printfn "Введите высоту:"
    let height = readInput()

    calculateCylinderVolume radius height |> printResult


printfn "Выберите режим (1 - суперпозиция, 2 - каррирование):"
match Console.ReadLine() with
| "1" -> mainSuperposition()
| "2" -> mainCurrying()
| _ -> printfn "Неверный ввод"


let rec sumDigits n =
    let absN = abs n 
    if absN = 0 then 0
    else (absN % 10) + sumDigits (absN / 10)


let readNumber() =
    printfn "Введите целое число:"
    match Console.ReadLine() |> Int32.TryParse with
    | (true, num) -> num
    | (false, _) ->
        printfn "Некорректный ввод"
        0


let main() =
    let number = readNumber()
    let result = sumDigits number
    printfn "Сумма цифр числа %d: %d" number result


main()










// Рекурсия вверх 
let rec sumDigitsUp n =
    let absN = abs n
    if absN = 0 then 0
    else (absN % 10) + sumDigitsUp (absN / 10)


// Хвостовая рекурсия 
let sumDigitsTail n =
    let rec helper acc num =
        let absNum = abs num
        if absNum = 0 then acc
        else helper (acc + absNum % 10) (absNum / 10)
    helper 0 n

let readNumber() =
    printfn "Введите целое число:"
    match Console.ReadLine() |> Int32.TryParse with
    | (true, num) -> num
    | _ -> 
        printfn "Некорректный ввод"
        0

let rec countDivisors n divisor z =
    match divisor with
    | d when d * d > n -> 
        z
    | d when n % d = 0 -> 
       countDivisors n (d + 1) (z + 1)
    | _ ->
        countDivisors n (divisor + 1) (z)
         
let countDivisorsOf n =
    countDivisors n 1 0


let main() =
    let number = readNumber()
    printfn "Рекурсия вверх: %d" (sumDigitsUp number)
    printfn "Хвостовая рекурсия: %d" (sumDigitsTail number)
    printfn "Число делителей: %d" (countDivisorsOf number)


main()


*)


// Дополнительные сведения о F# см. на http://fsharp.net
// Дополнительную справку см. в проекте "Учебник по F#".

(*let rec readList n = 
    if n=0 then []
    else
    let Head = System.Convert.ToInt32(System.Console.ReadLine())
    let Tail = readList (n-1)
    Head::Tail

let readData = 
    let n=System.Convert.ToInt32(System.Console.ReadLine())
    readList n

let rec writeList = function
    [] ->   let z = System.Console.ReadKey()
            0
    | (head : int)::tail -> 
                       System.Console.WriteLine(head)
                       writeList tail  

let max2 x y = if x > y then x else y
*)
let rec accCond list (f : int -> int -> int) p acc = 
    match list with
    | [] -> acc
    | h::t ->
                let newAcc = f acc h
                if p h then accCond t f p newAcc
                else accCond t f p acc

(*let listMin list = 
    match list with 
    |[] -> 0
    | h::t -> accCond list (fun x y -> if x < y then x else y) (fun x -> true) h

let listMax list = 
    match list with 
    |[] -> 0
    | h::t -> accCond list max2 (fun x -> true) h

let listSum list = accCond list (fun x y -> x + y) (fun x -> true) 0

let listPr list = accCond list (fun x y -> x * y) (fun x -> true) 1

 


let f6 list = 
    match list with
    |[] -> 0
    | h::t ->   if (h % 2 = 0) then accCond t max2 (fun x -> ((x % 2) = 0)) h
                else accCond t max2 (fun x -> ((x % 2) <> 0) ) h

let rec frequency list elem count =
        match list with
        |[] -> count
        | h::t -> 
                        let count1 = count + 1
                        if h = elem then frequency t elem count1 
                        else frequency t elem count

let rec freqList list mainList curList = 
        match list with
        | [] -> curList
        | h::t -> 
                    let freqElem = frequency mainList h 0
                    let newList = curList @ [freqElem]
                    freqList t mainList newList

let pos list el = 
    let rec pos1 list el num = 
        match list with
            |[] -> 0
            |h::t ->    if (h = el) then num
                        else 
                            let num1 = num + 1
                            pos1 t el num1
    pos1 list el 1

let getIn list pos = 
    let rec getIn1 list num curNum = 
        match list with 
            |[] -> 0
            |h::t -> if num = curNum then h
                     else 
                            let newNum = curNum + 1
                            getIn1 t num newNum
    getIn1 list pos 1

let f7 list = 
    let fL = freqList list list []
    (listMax fL) |> (pos fL) |> (getIn list)           

let filter list pr = 
    let rec filter1 list pr newList = 
        match list with
        | [] -> newList
        | h::t ->
                let newnewList = newList @ [h]
                if pr h then filter1 list pr newnewList
                else filter1 list pr newList
    filter1 list pr [] 

let even n = ((n % 2) = 0)

let f8Cond list el = (even el) && (even (frequency list el 0))

let f8 list = filter list (f8Cond list)

let delEL list el = filter list (fun x -> (x <> el))

let uniq list = 
    let rec uniq1 list newList = 
        match list with
            |[] -> newList
            | h::t -> 
                        let listWithout = delEL t h
                        let newnewList = newList @ [h]
                        uniq1 listWithout newnewList
    uniq1 list [] 

let rec cifrSum n = 
    if n = 0 then 0
    else (n%10) + (cifrSum (n / 10))

let f9Cond el = ((cifrSum el) > 9) || (even el)

let f9 list = filter list f9Cond

let count x y = x + 1

let f10Cond list El = (accCond list count (fun x -> ((x * x) = El)) 0) > 0

let f10 list = accCond list count (f10Cond list) 0   

let main argv = 
    writeList readData
    let l = readData
    let sum : int = listSum l
    let pr : int= listPr l
    let min : int= listMin l
    let max : int= listMax l
    System.Console.WriteLine((sum,pr,min,max))
    
    let ans = f7 l
    System.Console.WriteLine(ans)
    let z = System.Console.ReadKey()
*)

type BinaryTree<'T> =
    | Empty
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>

let rec insert x tree =
    match tree with
    | Empty -> Node(x, Empty, Empty)
    | Node(v, left, right) when x < v -> Node(v, insert x left, right)
    | Node(v, left, right) when x > v -> Node(v, left, insert x right)
    | _ -> tree  

let insertMany values =
    List.fold (fun acc x -> insert x acc) Empty values

let rec inorder tree =
    match tree with
    | Empty -> []
    | Node(v, left, right) ->
        inorder left @ [v] @ inorder right

let rec search x tree =
    match tree with
    | Empty -> false
    | Node(v, left, right) when x = v -> true
    | Node(v, left, right) when x < v -> search x left
    | Node(_, _, right) -> search x right

[<EntryPoint>]
let main argv =
    let values = [5; 3; 8; 1; 4; 7; 9]
    let tree = insertMany values

    printfn "Поиск 4: %b" (search 4 tree)
    printfn "Поиск 6: %b" (search 6 tree)
    printfn "In-order обход: %A" (inorder tree)

    0

