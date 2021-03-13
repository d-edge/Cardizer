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

let private getNumber n =
    let n2 = n * 2
    if n2 > 9 then n2 - 9 else n2

let private sumDigit n length =
    if length % 2 = 0 then n else getNumber n

let rec private getNumbers digits sum length =
    if length = 0 then
        (sum, digits)
    else
        let n = nextDigit ()
        getNumbers (digits @ [ n ]) (sum + (sumDigit n length)) (length - 1)

let inline private checkDigit sum = ((sum / 10 + 1) * 10 - sum) % 10

let private digitsToCard sum digits =
    digits @ [ checkDigit sum ]
    |> List.fold (fun r n -> r + (string n)) String.Empty

let generateVisa () =
    let sum, digits = getNumbers [ 4 ] 8 14

    digitsToCard sum digits

let generateAmex () =
    let a, b = if next 2 = 0 then 4,8 else 7,5
    let sum, digits = getNumbers [ 3; a ] (3 + b) 12

    digitsToCard sum digits
