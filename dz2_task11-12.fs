let favoriteLanguage language = 
    match language with
    | "F#" | "Prolog" -> "Ты - подлиза ~_~"
    | "Python" -> "Выбор большинства ¬_¬"
    | "Rust" | "Haskell" -> "Скрываешься от систем статического анализа ^_-"
    | "C" | "C++" -> "Ок, дед;"
    | "JS" -> "Понял, ты вебщик"
    | _ -> "Интересный выбор"

let main_superposition () =
    printf "Какой твой любимый язык программирования: "
    let userInput = System.Console.ReadLine()
    let result = (printf "%s") << favoriteLanguage
    result userInput

let main_currying () =
    let askFavoriteLanguage () =
        printf "Какой твой любимый язык программирования: "
        System.Console.ReadLine()
    
    let displayResult result =
        printf "%s" result

    let curriedFunction = favoriteLanguage >> displayResult
    curriedFunction (askFavoriteLanguage ())

main_superposition ()

main_currying ()