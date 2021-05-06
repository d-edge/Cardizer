namespace Dedge

open System
open System.Threading
open System.Runtime.InteropServices

type VisaLengthOptions =
    | Random = 0
    | Thirteen = 1
    | Sixteen = 2

type Cardizer =

    // original snippet by @tpetricek
    // https://stackoverflow.com/a/7792667/1248177
    static member private next =
        let seedGenerator = Random()

        let localGenerator =
            new ThreadLocal<Random>(fun _ ->
                lock
                    seedGenerator
                    (fun _ ->
                        let seed = seedGenerator.Next()
                        Random(seed)))

        fun n -> localGenerator.Value.Next(n)

    static member private getNumber n =
        let n2 = n * 2
        if n2 > 9 then n2 - 9 else n2

    static member private sumDigit i n = if i then n else Cardizer.getNumber n

    static member inline private checkDigit sum = ((sum / 10 + 1) * 10 - sum) % 10

    static member private nextUniquePersonalIdentifiers n =
        { 1 .. n } |> Seq.map (fun _ -> Cardizer.next 10)

    static member private sumFold length state digits =
        digits
        |> Seq.mapi (fun i n -> (length + i) % 2 = 0, n)
        |> Seq.mapFold (fun sum (isEven, n) -> n, sum + Cardizer.sumDigit isEven n) state

    static member private assemble prefixes (numbers, sum) =
        seq {
            yield! prefixes
            yield! numbers
            yield Cardizer.checkDigit sum
        }

    static member private generateCard (prefixes: seq<int>) state cardLen =
        let checksumLen = 1
        let prefixesLen = Seq.length prefixes
        let generateLen = cardLen - prefixesLen - checksumLen
        let shift = (cardLen - (prefixesLen % 2 - 1)) % 2

        generateLen
        |> Cardizer.nextUniquePersonalIdentifiers
        |> Cardizer.sumFold shift state
        |> Cardizer.assemble prefixes
        |> String.Concat

    /// <summary>Returns a random Visa number that is of the given available length.</summary>
    /// <param name="visaLengthOption">Credit card's length (default is randomized between 13 and 16)</param>
    /// <returns>Random Visa number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextVisa"/> method.
    /// <code>
    /// void PrintVisa()
    /// {
    ///    Console.WriteLine(Cardizer.NextVisa()); // randomized between 13 or 16
    ///    Console.WriteLine(Cardizer.NextVisa(VisaLengthOptions.Random)); // randomized between 13 or 16
    ///    Console.WriteLine(Cardizer.NextVisa(VisaLengthOptions.Thirteen));
    ///    Console.WriteLine(Cardizer.NextVisa(VisaLengthOptions.Sixteen));
    /// }
    /// </code>
    /// </example>
    static member NextVisa
        ([<Optional; DefaultParameterValue(VisaLengthOptions.Random)>] visaLengthOption: VisaLengthOptions)
        =
        let (sum, length) =
            match visaLengthOption with
            | VisaLengthOptions.Thirteen -> 4, 13
            | VisaLengthOptions.Sixteen -> 8, 16
            | _ ->
                if Cardizer.next 2 = 0 then
                    4, 13
                else
                    8, 16

        Cardizer.generateCard [ 4 ] sum length

    static member NextVerve() =
        let nextInRange start stop =
            Cardizer.next (stop - start + 1) + start

        let numberToSeq number =
            let rec loop n list =
                if n <= 0 then
                    list
                else
                    loop (n / 10) (n % 10 :: list)

            loop number []

        let length = 16 + 3 * Cardizer.next 2

        let prefix =
            [ [ 506099; 506198 ]
              [ 650002; 650027 ] ].[Cardizer.next 2]

        let shift = (length + 1) % 2

        let prefixes, state =
            nextInRange prefix.[0] prefix.[1]
            |> numberToSeq
            |> Cardizer.sumFold shift 0

        Cardizer.generateCard prefixes state length

    static member NextMir() =
        let fourth = Cardizer.next 5
        Cardizer.generateCard [ 2; 2; 0; fourth ] (6 + fourth) 16

    static member NextJcb() =
        let third = Cardizer.next 7 + 2
        let fourth = Cardizer.next 2 + 8
        Cardizer.generateCard [ 3; 5; third; fourth ] (11 + (Cardizer.getNumber third) + fourth) 16

    static member NextAmex() =
        let a, b =
            if Cardizer.next 2 = 0 then
                4, 8
            else
                7, 5

        Cardizer.generateCard [ 3; a ] (3 + b) 15

    static member NextDiscover() =
        Cardizer.generateCard [ 6; 0; 1; 1 ] 6 16

    static member NextMasterCard() =
        let second = Cardizer.next 4 + 1
        Cardizer.generateCard [ 5; second ] (1 + second) 16
