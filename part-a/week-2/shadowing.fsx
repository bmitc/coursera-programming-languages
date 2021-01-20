// F# does not explicitly supporting shadowing like SML!
// You can only shadow in FSI or inside a function definition.
// Running this code will generate a "Duplicate definition" error.
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#scope

let a = 10

let b = a * 2

let a = 5

let c = b

let d = a

let a = a + 1

let f = a * 2