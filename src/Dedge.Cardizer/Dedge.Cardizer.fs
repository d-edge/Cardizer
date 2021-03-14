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

let private sumDigit n length =
    let n2 = n * 2

    if length % 2 = 0 then n
    else if n2 > 9 then n2 - 9
    else n2

// let rec private getNumbers digits sum length =
//     if length = 0 then
//         (sum, digits)
//     else
//         let n = nextDigit ()
//         getNumbers (digits @ [ n ]) (sum + (sumDigit n length)) (length - 1)

// let private getNumbers2 digits sum length =
//     let numbers =
//         [ 0 .. length - 1 ]
//         |> List.map (fun _ -> nextDigit ())

//     let result =
//         numbers
//         |> List.mapi (fun i n -> (i, n))
//         |> List.fold (fun acc (i, n)-> acc + (sumDigit n i)) sum
    
//     (result, digits @ numbers)

let private getNumbers3 digits sum length =
    let numbers, result =
        [ 0 .. length - 1 ]
        |> List.map (fun i -> (i, nextDigit ()))
        |> List.mapFold (fun acc (i, n) -> (n, acc + (sumDigit n i))) sum

    (result, digits @ numbers)

let inline private checkDigit sum = ((sum / 10 + 1) * 10 - sum) % 10

let private digitsToCard sum digits =
    digits @ [ checkDigit sum ]
    |> List.fold (fun r n -> r + (string n)) String.Empty

let generateVisa () =
    let sum, digits = getNumbers3 [ 4 ] 8 14

    digitsToCard sum digits
