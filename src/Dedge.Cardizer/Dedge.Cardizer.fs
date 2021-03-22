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

let private applySnd f (a, b) = a, f b

let private getNumber n =
    let n2 = n * 2
    if n2 > 9 then n2 - 9 else n2

let private sumDigit i n = if i then n else getNumber n

let inline private checkDigit sum = ((sum / 10 + 1) * 10 - sum) % 10

let private nextUniquePersonalIdentifiers n =
    [ 1 .. n ] |> List.map (fun _ -> next 10)

let private sumFold length state digits =
    digits
    |> List.mapi (fun i n -> (length + i) % 2 = 0, n)
    |> List.mapFold (fun sum (isEven, n) -> n, sum + sumDigit isEven n) state

let private generateCard (prefixes: list<int>) state cardLength =
    let checksumLength = 1

    let lengthToGenerate =
        cardLength - prefixes.Length - checksumLength

    let shift =
        (cardLength - (prefixes.Length % 2 - 1)) % 2

    lengthToGenerate
    |> nextUniquePersonalIdentifiers
    |> sumFold shift state
    |> applySnd (checkDigit >> string)
    |> fun (numbers, sum) -> (prefixes @ numbers |> String.Concat) + sum

type VisaLengthOptions =
    | Random = 0
    | Thirteen = 1
    | Sixteen = 2

let constructVisa visaLengthOptions =
    let (sum, length) =
        match visaLengthOptions with
        | VisaLengthOptions.Thirteen -> 4, 13
        | VisaLengthOptions.Sixteen -> 8, 16
        | _ -> 8, 16

    generateCard [ 4 ] sum length

let generateVisa () = constructVisa VisaLengthOptions.Random

let generateVerve () =
    let nextInRange start stop = next (stop - start + 1) + start
    let charToInt c = int c - 48
    let numberToSeq n = n |> string |> Seq.map charToInt
    let length = 16 + 3 * next 2

    let prefix =
        [ [ 506099; 506198 ]
          [ 650002; 650027 ] ].[next 2]

    let shift = (length + 1) % 2

    let prefixes, state =
        nextInRange prefix.[0] prefix.[1]
        |> numberToSeq
        |> Seq.toList
        |> sumFold shift 0

    generateCard prefixes state length

let generateMir () =
    let fourth = next 5
    generateCard [ 2; 2; 0; fourth ] (6 + fourth) 16

let generateJcb () =
    let third = next 7 + 2
    let fourth = next 2 + 8
    generateCard [ 3; 5; third; fourth ] (11 + (getNumber third) + fourth) 16

let generateAmex () =
    let a, b = if next 2 = 0 then 4, 8 else 7, 5
    generateCard [ 3; a ] (3 + b) 15

let generateDiscover () = generateCard [ 6; 0; 1; 1 ] 6 16

let generateMasterCard () =
    let second = next 4 + 1
    generateCard [ 5; second ] (1 + second) 16
