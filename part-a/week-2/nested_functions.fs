// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/code-formatting-guidelines
// https://fsharpforfunandprofit.com/posts/fsharp-syntax/

let countupFromOne (x : int) =
    let rec count (from : int) =
        if from = x
        then x :: []
        else from :: count(from+1)
    count(1)