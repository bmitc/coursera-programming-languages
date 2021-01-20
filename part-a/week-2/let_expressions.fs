// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/let-bindings

let silly1 (z : int) =
    let x = if z > 0 then z else 34
    let y = x + z + 9
    if x > y then x * 2 else y * y

// Note that F# allows you to shadow bindings inside
// function definitions.

let silly2 () =
    let x = 1
    (let x = 2 in x+1) + (let y = x + 2 in y+1)

// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/keyword-reference
// The end keyword in F# is different than in SML.
// F# uses indentation and parantheses for code blocks

let silly3 () =
    let x = (let x = 5 in x + 10)
    (x, (let x = 2 in x), let x = 10 in (let x = 2 in x))