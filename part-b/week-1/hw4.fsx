// Takes low, high, and stride and returns a list of numbers from low to
// a number not exceeding high incremented with stride. If high is less
// than low, then return the empty list.
let rec sequence low high stride =
    if (low > high)
    then List.empty
    else low :: (sequence (low + stride) high stride)

// Takes a list of strings and a suffix and returns a list of each element
// of the original list with suffix appened to the end of the element.
let stringAppendMap xs suffix =
    List.map (fun (s : string) -> s + suffix) xs

// Takes a list and a number n and returns the ith element where i is the
// remainder produced by dividing n by the list's length. Errors are returned
// if n is negative or the list is empty.
let listNthMod xs n =
    if n < 0 then raise (System.ArgumentException "listNthMod: negative number")
    elif List.isEmpty xs then raise (System.ArgumentException "listNthMod: empty list")
    else
        let rem = n % (xs.Length)
        List.item rem xs

// A stream of the natural numbers: 1, 2, 3, ...
// Used for testing other functions that operate on streams.
// Taken from the course video lectures.
let nats = Seq.initInfinite (fun x -> x + 1)

// Takes a stream and a number n and returns a list holdig the first n
// elements of the stream. For example, streamForNSteps nats 5 = [1;2;3;4;5].
let rec streamForNSteps s n =
    if n = 0
    then List.empty
    else
        let (head, tail) = (Seq.head s, Seq.tail s)
        head :: streamForNSteps tail (n - 1)

// A stream identical to the natural numbers except multiples of 5 are negated.
// For example: 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...
let funnyNumberStream = Seq.initInfinite (fun x ->
    let y = x + 1
    if (y % 5) = 0
    then -y
    else y)

// Stream that produces "dan.jpg", "dog.jpg", "dan.jpg", "dog.jpg", ...
let danThenDog = Seq.initInfinite (fun x ->
    if x % 2 = 0
    then "dan.jpg"
    else "dog.jpg")

// Takes a stream and returns a new stream with each element being a pair
// (0 . v) where v is the value of each original element.
let streamAddZero s = Seq.map (fun x -> (0, x)) s

// Takes two lists and returns a stream where each value returned by the stream
// is a pair made up of an element from each list. The list elements are cycled
// through and then wrap back around.
let cycleLists xs ys =
    Seq.initInfinite (fun n -> (listNthMod xs n), (listNthMod ys n))