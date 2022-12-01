type Card = { Suit: string; Value: int }

let fourOfHears =
    { Suit = "hearts"; Value = 4 }
    
type Temperature =
    | DegreesC of int
    | DegreesF of int

let printTemp (temp: Temperature) =
    match temp with
     | DegreesC t -> printfn $"{t} degrees c"
     | DegreesF t -> printfn $"{t} degrees f"

let sumNumbersUpTo max =
    let rec recursiveSum n partialSum =
        match n with
        | 0 -> partialSum
        | _ -> recursiveSum (n - 1) (partialSum + n)
    recursiveSum max 0

[<EntryPoint>]
let main _ =
    let celcius = DegreesC 32
    let farenheight = DegreesF 100
    printTemp celcius
    printTemp farenheight
    printfn $"{sumNumbersUpTo 5}"
    0
