module Cardizer

open System
open System.Threading

// https://stackoverflow.com/a/7792667/1248177
let next =
    // Create master seed generator and thread local value
    let seedGenerator = Random()

    let localGenerator =
        new ThreadLocal<Random>(fun _ ->
            lock
                seedGenerator
                (fun _ ->
                    let seed = seedGenerator.Next()
                    Random(seed)))
    // Return function that uses thread local random generator
    fun n -> localGenerator.Value.Next(n)

let private nextDigit () = next 10

let private getVisaSum n i =
    let n2 = n * 2

    if i % 2 = 0 then n
    else if n2 > 9 then n2 - 9
    else n2

let rec private getVisaNumbers digits sum i =
    if i = 14 then
        (digits, sum)
    else
        let n = nextDigit ()
        getVisaNumbers (digits @ [ n ]) (sum + (getVisaSum n i)) (i + 1)

let private getLast sum =
    if sum % 10 = 0 then
        "0"
    else
        10 - (sum % 10) |> string

let private digitsToCard sum digits =
    let numbers = 
       digits
        |> List.fold (fun r n3 -> r + (string n3)) String.Empty
    numbers + getLast sum
    

let generateVisa () =
    let digits, sum = getVisaNumbers [ 4 ] 8 0

    digitsToCard sum digits
