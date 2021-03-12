module Dedge.Cardizer

open System
open System.Threading

// original snippet by @tpetricek
// https://stackoverflow.com/a/7792667/1248177
let private next =
    let seedGenerator = Random()

    let localGenerator =
        new ThreadLocal<Random>(fun _ ->
            lock
                seedGenerator
                (fun _ ->
                    let seed = seedGenerator.Next()
                    Random(seed)))

    fun n -> localGenerator.Value.Next(n)

let private nextDigit () = next 10

let private sumDigit n i =
    let n2 = n * 2

    if i % 2 = 0 then n
    else if n2 > 9 then n2 - 9
    else n2

let rec private getVisaNumbers length digits sum i =
    if i = length then
        (sum, digits)
    else
        let n = nextDigit ()
        getVisaNumbers length (digits @ [ n ]) (sum + (sumDigit n i)) (i + 1)

let inline private checkDigit sum = ((sum / 10 + 1) * 10 - sum) % 10

let private digitsToCard sum digits =
    digits @ [ checkDigit sum ]
    |> List.fold (fun r n -> r + (string n)) String.Empty

let generateVisa () =
    let sum, digits = getVisaNumbers 14 [ 4 ] 8 0

    digitsToCard sum digits
