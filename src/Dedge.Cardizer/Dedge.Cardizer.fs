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

let applySnd f (a, b) = a, f b

let private getNumber n =
    let n2 = n * 2
    if n2 > 9 then n2 - 9 else n2

let private sumDigit i n = if i % 2 = 0 then n else getNumber n

let inline private checkDigit sum = ((sum / 10 + 1) * 10 - sum) % 10

let private getNumbers state length =
    [ length .. -1 .. 1 ]
    |> List.map (fun i -> i, nextDigit ())
    |> List.mapFold (fun sum (i, n) -> n, sum + sumDigit i n) state
    |> applySnd (checkDigit >> string)

let private generateCard prefixes state length =
    getNumbers state length
    |> fun (numbers, sum) -> (prefixes @ numbers |> String.Concat) + sum

let generateVisa () = generateCard [ 4 ] 8 14

let generateAmex () =
    let a, b = if next 2 = 0 then 4, 8 else 7, 5
    generateCard [ 3; a ] (3 + b) 12
