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

let generateVerve () =
    let nextInRange start stop = next (stop - start + 1) + start
    let charToInt c = int c - 48
    let numberToSeq n = n |> string |> Seq.map charToInt
    let length = 9 + 3 * next 2

    let prefix =
        [ [ 506099; 506198 ]
          [ 650002; 650027 ] ].[next 2]

    let prefixes, state =
        nextInRange prefix.[0] prefix.[1]
        |> numberToSeq
        |> Seq.mapi (fun i n -> length + i, n)
        |> Seq.mapFold (fun s (i, n) -> n, s + sumDigit i n) 0

    generateCard (Seq.toList prefixes) state length
