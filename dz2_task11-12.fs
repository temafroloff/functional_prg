let favoriteLanguage language = 
    match language with
    | "F#" | "Prolog" -> "�� - ������� ~_~"
    | "Python" -> "����� ����������� �_�"
    | "Rust" | "Haskell" -> "����������� �� ������ ������������ ������� ^_-"
    | "C" | "C++" -> "��, ���;"
    | "JS" -> "�����, �� ������"
    | _ -> "���������� �����"

let main_superposition () =
    printf "����� ���� ������� ���� ����������������: "
    let userInput = System.Console.ReadLine()
    let result = (printf "%s") << favoriteLanguage
    result userInput

let main_currying () =
    let askFavoriteLanguage () =
        printf "����� ���� ������� ���� ����������������: "
        System.Console.ReadLine()
    
    let displayResult result =
        printf "%s" result

    let curriedFunction = favoriteLanguage >> displayResult
    curriedFunction (askFavoriteLanguage ())

main_superposition ()

main_currying ()