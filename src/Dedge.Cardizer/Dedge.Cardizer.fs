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

let private sumDigit n i = if i % 2 = 0 then n else getNumber n

let private getNumbers state length =
    [ length .. -1 .. 1 ]
    |> List.map (fun i -> (i, nextDigit ()))
    |> List.mapFold (fun sum (i, n) -> (n, sum + (sumDigit n i))) state

let inline private checkDigit sum = ((sum / 10 + 1) * 10 - sum) % 10

let private generateCard prefixes state length =
    let numbers, sum = getNumbers state length

    (prefixes @ numbers @ [ checkDigit sum ])
    |> List.fold (fun r n -> r + (string n)) String.Empty

let generateVisa () = generateCard [ 4 ] 8 14
