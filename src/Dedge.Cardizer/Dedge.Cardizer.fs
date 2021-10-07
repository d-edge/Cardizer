namespace Dedge

open System
open System.Threading
open System.Runtime.InteropServices

type VisaLengthOptions =
    | Random = 0
    | Thirteen = 13
    | Sixteen = 16

type VerveLengthOptions =
    | Random = 0
    | Sixteen = 16
    | Nineteen = 19

type From12To19 =
    | Random = 0
    | Twelve = 12
    | Thirteen = 13
    | Fourteen = 14
    | Fifteen = 15
    | Sixteen = 16
    | Seventeen = 17
    | Eightteen = 18
    | Nineteen = 19

type From16To19 =
    | Random = 0
    | Sixteen = 16
    | Seventeen = 17
    | Eightteen = 18
    | Nineteen = 19
    
type From16To19Skip17 =
    | Random = 0
    | Sixteen = 16
    | Eighteen = 18
    | Nineteen = 19

type DinersClubInternationalLengthOptions =
    | Random = 0
    | Fourteen = 14
    | Fifteen = 15
    | Sixteen = 16
    | Seventeen = 17
    | Eightteen = 18
    | Nineteen = 19

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

    /// <summary>Returns a random integer within a given range.</summary>
    /// <param name="low">The (inclusive) low value of the range</param>
    /// <param name="high">The (inclusive) high value of the range</param>
    /// <returns>Random integer within a given range</returns>
    static member private NextInRange low high = Cardizer.next (high - low + 1) + low

    /// <summary>Returns a sequence of each digit of a given number.</summary>
    /// <param name="number">The number to enumerate</param>
    /// <returns>A sequence of each digit of a given number</returns>
    static member private NumberToSeq number =
        let rec loop n list =
            if n <= 0 then
                list
            else
                loop (n / 10) (n % 10 :: list)

        loop number []

    /// <summary>Returns a random integer enumerate as sequence within a given range.</summary>
    /// <param name="low">The (inclusive) low value of the range</param>
    /// <param name="high">The (inclusive) high value of the range</param>
    /// <returns>Random integer enumerate as sequence within a given range</returns>
    static member private NextSeqInRange low high =
        Cardizer.NextInRange low high
        |> Cardizer.NumberToSeq

    static member private GetNumber n =
        let n2 = n * 2
        if n2 > 9 then n2 - 9 else n2

    static member inline private CheckDigit sum = ((sum / 10 + 1) * 10 - sum) % 10

    static member private NextUniquePersonalIdentifiers n =
        { 1 .. n } |> Seq.map (fun _ -> Cardizer.next 10)

    static member private ReverseSum(digits: seq<int>) : int =
        digits
        |> Seq.rev
        |> Seq.mapi (fun i n -> i % 2 = 0, n)
        |> Seq.sumBy
            (fun (isEven, n) ->
                if isEven then
                    Cardizer.GetNumber n
                else
                    n)
        |> Cardizer.CheckDigit

    static member private AppendSum digits =
        seq {
            yield! digits
            yield Cardizer.ReverseSum digits
        }

    static member private GenerateCard (prefixes: seq<int>) cardLen =
        let checksumLen = 1
        let prefixesLen = Seq.length prefixes
        let generateLen = cardLen - prefixesLen - checksumLen

        generateLen
        |> Cardizer.NextUniquePersonalIdentifiers
        |> Seq.toList
        |> Seq.append prefixes
        |> Cardizer.AppendSum
        |> String.Concat

    /// <summary>Returns a random Visa number that is of the given available length.</summary>
    /// <param name="visaLengthOption">Credit card's length (default is randomized between 13 or 16)</param>
    /// <returns>Random Visa number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextVisa"/> method.
    /// <code>
    /// void PrintVisa()
    /// {
    ///    Console.WriteLine(Cardizer.NextVisa()); // randomized between 13 or 16
    ///    Console.WriteLine(Cardizer.NextVisa(VisaLengthOptions.Random)); // randomized between 13 or 16
    ///    Console.WriteLine(Cardizer.NextVisa(VisaLengthOptions.Sixteen));
    /// }
    /// </code>
    /// </example>
    static member NextVisa([<Optional; DefaultParameterValue(VisaLengthOptions.Random)>] visaLengthOption) =
        let length =
            match visaLengthOption with
            | VisaLengthOptions.Random -> if Cardizer.next 2 = 0 then 13 else 16
            | _ -> int visaLengthOption


        Cardizer.GenerateCard [ 4 ] length

    /// <summary>Returns a random Verve number that is of the given available length.</summary>
    /// <param name="verveLengthOption">Credit card's length (default is randomized between 16 or 19)</param>
    /// <returns>Random Verve number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextVerve"/> method.
    /// <code>
    /// void PrintVerve()
    /// {
    ///    Console.WriteLine(Cardizer.NextVerve()); // randomized between 16 or 19
    ///    Console.WriteLine(Cardizer.NextVerve(VerveLengthOptions.Random)); // randomized between 16 or 19
    ///    Console.WriteLine(Cardizer.NextVerve(VerveLengthOptions.Sixteen));
    /// }
    /// </code>
    /// </example>
    static member NextVerve([<Optional; DefaultParameterValue(VerveLengthOptions.Random)>] verveLengthOption) =
        let length =
            match verveLengthOption with
            | VerveLengthOptions.Random -> 16 + 3 * Cardizer.next 2
            | _ -> int verveLengthOption

        let prefix =
            [ [ 506099; 506198 ]
              [ 650002; 650027 ] ].[Cardizer.next 2]

        let prefixes =
            Cardizer.NextInRange prefix.[0] prefix.[1]
            |> Cardizer.NumberToSeq

        Cardizer.GenerateCard prefixes length

    /// <summary>Returns a random Mir number that is of the given available length.</summary>
    /// <param name="mirLengthOption">Credit card's length (default is randomized between 16 and 19)</param>
    /// <returns>Random Mir number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextMir"/> method.
    /// <code>
    /// void PrintMir()
    /// {
    ///    Console.WriteLine(Cardizer.NextMir()); // randomized between 16 and 19
    ///    Console.WriteLine(Cardizer.NextMir(MirLengthOptions.Random)); // randomized between 16 and 19
    ///    Console.WriteLine(Cardizer.NextMir(MirLengthOptions.Sixteen));
    /// }
    /// </code>
    /// </example>
    static member NextMir([<Optional; DefaultParameterValue(From16To19.Random)>] mirLengthOption) =
        let length =
            match mirLengthOption with
            | From16To19.Random -> Cardizer.NextInRange 16 19
            | _ -> int mirLengthOption

        let prefixes = [ 2; 2; 0; Cardizer.next 5 ]

        Cardizer.GenerateCard prefixes length

    /// <summary>Returns a random Jcb number that is of the given available length.</summary>
    /// <param name="jcbLengthOption">Credit card's length (default is randomized between 16 and 19)</param>
    /// <returns>Random Jcb number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextJcb"/> method.
    /// <code>
    /// void PrintJcb()
    /// {
    ///    Console.WriteLine(Cardizer.NextJcb()); // randomized between 16 and 19
    ///    Console.WriteLine(Cardizer.NextJcb(JcbLengthOptions.Random)); // randomized between 16 and 19
    ///    Console.WriteLine(Cardizer.NextJcb(JcbLengthOptions.Sixteen));
    /// }
    /// </code>
    /// </example>
    static member NextJcb([<Optional; DefaultParameterValue(From16To19.Random)>] jcbLengthOption) =
        let length =
            match jcbLengthOption with
            | From16To19.Random -> Cardizer.NextInRange 16 19
            | _ -> int jcbLengthOption

        let prefixes =
            [ 3
              5
              Cardizer.NextInRange 2 8
              Cardizer.NextInRange 8 9 ]

        Cardizer.GenerateCard prefixes length

    /// <summary>Returns a random Amex number.</summary>
    /// <returns>Random Amex number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextAmex"/> method.
    /// <code>
    /// void PrintAmex()
    /// {
    ///    Console.WriteLine(Cardizer.NextAmex());
    /// }
    /// </code>
    /// </example>
    static member NextAmex() =
        let second = if Cardizer.next 2 = 0 then 4 else 7
        Cardizer.GenerateCard [ 3; second ] 15

    /// <summary>Returns a random Discover number that is of the given available length.</summary>
    /// <param name="discoverLengthOption">Credit card's length (default is randomized between 16 and 19)</param>
    /// <returns>Random Discover number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDiscover"/> method.
    /// <code>
    /// void PrintDiscover()
    /// {
    ///    Console.WriteLine(Cardizer.NextDiscover()); // randomized between 16 and 19
    ///    Console.WriteLine(Cardizer.NextDiscover(DiscoverLengthOptions.Random)); // randomized between 16 and 19
    ///    Console.WriteLine(Cardizer.NextDiscover(DiscoverLengthOptions.Sixteen));
    /// }
    /// </code>
    /// </example>
    static member NextDiscover([<Optional; DefaultParameterValue(From16To19.Random)>] discoverLengthOption) =
        let length =
            match discoverLengthOption with
            | From16To19.Random -> Cardizer.NextInRange 16 19
            | _ -> int discoverLengthOption

        let roll = Cardizer.next 4

        let prefix =
            if roll = 0 then
                [ 6; 0; 1; 1 ]
            elif roll = 1 then
                Cardizer.NextSeqInRange 622126 622925
            elif roll = 2 then
                Cardizer.NextSeqInRange 644 649
            else
                [ 6; 5 ]

        Cardizer.GenerateCard prefix length

    /// <summary>Returns a random MasterCard number.</summary>
    /// <returns>Random MasterCard number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextMasterCard"/> method.
    /// <code>
    /// void PrintMasterCard()
    /// {
    ///    Console.WriteLine(Cardizer.NextMasterCard());
    /// }
    /// </code>
    /// </example>
    static member NextMasterCard() =
        let prefixes =
            if Cardizer.next 2 = 0 then
                Cardizer.NextSeqInRange 51 55
            else
                Cardizer.NextSeqInRange 2221 2720

        Cardizer.GenerateCard prefixes 16


    /// <summary>Returns a random Uatp number.</summary>
    /// <returns>Random Uatp number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextUatp"/> method.
    /// <code>
    /// void PrintUatp()
    /// {
    ///    Console.WriteLine(Cardizer.NextUatp());
    /// }
    /// </code>
    /// </example>
    static member NextUatp() = Cardizer.GenerateCard [ 1 ] 15

    /// <summary>Returns a random RuPay number.</summary>
    /// <returns>Random RuPay number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextRuPay"/> method.
    /// <code>
    /// void PrintRuPay()
    /// {
    ///    Console.WriteLine(Cardizer.NextRuPay());
    /// }
    /// </code>
    /// </example>
    static member NextRuPay([<Optional; DefaultParameterValue(true)>] acceptCoBranded: bool) =
        let prefixRuPay =
            [ [ 6; 0 ]
              [ 6; 5 ]
              [ 8; 1 ]
              [ 8; 2 ]
              [ 5; 0; 8 ] ]

        let prefixRuPayAndJcbCobranded = [ [ 3; 5; 3 ]; [ 3; 5; 6 ] ]

        if acceptCoBranded then
            let merge =
                [ prefixRuPay
                  prefixRuPayAndJcbCobranded ].[Cardizer.next 2]

            if merge.Length = 2 then
                Cardizer.GenerateCard merge.[Cardizer.next 2] 16
            else
                Cardizer.GenerateCard merge.[Cardizer.next 5] 16
        else
            Cardizer.GenerateCard prefixRuPay.[Cardizer.next 5] 16

    /// <summary>Returns a random DinersClubInternational number.</summary>
    /// <returns>Random DinersClubInternational number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDinersClubInternational"/> method.
    /// <code>
    /// void PrintDinersClubInternational()
    /// {
    ///    Console.WriteLine(Cardizer.NextDinersClubInternational());
    /// }
    /// </code>
    /// </example>
    static member NextDinersClubInternational
        ([<Optional; DefaultParameterValue(DinersClubInternationalLengthOptions.Random)>] dinersLengthOption)
        =
        let length =
            match dinersLengthOption with
            | DinersClubInternationalLengthOptions.Random -> Cardizer.NextInRange 14 19
            | _ -> int dinersLengthOption

        Cardizer.GenerateCard [ 3; 6 ] length

    /// <summary>Returns a random DinersClubUsAndCanada number.</summary>
    /// <returns>Random DinersClubUsAndCanada number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDDinersClubUsAndCanada"/> method.
    /// <code>
    /// void PrintDinersClubUsAndCanada()
    /// {
    ///    Console.WriteLine(Cardizer.NextDinersClubUsAndCanada());
    /// }
    /// </code>
    /// </example>
    static member NextDinersClubUsAndCanada() = Cardizer.GenerateCard [ 5; 4 ] 16

    /// <summary>Returns a random DinersClubInternational or DinersClubUsAndCanada number.</summary>
    /// <returns>Random DinersClubInternational or DinersClubUsAndCanada number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDinersClub"/> method.
    /// <code>
    /// void PrintDinersClub()
    /// {
    ///    Console.WriteLine(Cardizer.NextDinersClub());
    /// }
    /// </code>
    /// </example>
    static member NextDinersClub() =
        if Cardizer.next 2 = 0 then
            Cardizer.NextDinersClubUsAndCanada()
        else
            Cardizer.NextDinersClubInternational()

    /// <summary>Returns a random Maestro number.</summary>
    /// <returns>Random Maestro number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextMaestro"/> method.
    /// <code>
    /// void PrintMaestro()
    /// {
    ///    Console.WriteLine(Cardizer.NextMaestro());
    /// }
    /// </code>
    /// </example>
    static member NextMaestro([<Optional; DefaultParameterValue(From12To19.Random)>] maestroLengthOption) =
        let length =
            match maestroLengthOption with
            | From12To19.Random -> Cardizer.NextInRange 12 19
            | _ -> int maestroLengthOption

        let prefix =
            [ [ 5; 0; 1; 8 ]
              [ 5; 0; 2; 0 ]
              [ 5; 0; 3; 8 ]
              [ 5; 8; 9; 3 ]
              [ 6; 3; 0; 4 ]
              [ 6; 7; 5; 9 ]
              [ 6; 7; 6; 1 ]
              [ 6; 7; 6; 2 ]
              [ 6; 7; 6; 3 ] ].[Cardizer.next 9]

        Cardizer.GenerateCard prefix length

    /// <summary>Returns a random Dankort number.</summary>
    /// <returns>Random Dankort number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDankort"/> method.
    /// <code>
    /// void PrintDankort()
    /// {
    ///    Console.WriteLine(Cardizer.NextDankort());
    /// }
    /// </code>
    /// </example>
    static member NextDankort([<Optional; DefaultParameterValue(true)>] acceptCoBranded: bool) =
        let prefixDankort = [ 5; 0; 1; 9 ]
        let prefixVisaCobranded = [ 4; 5; 7; 1 ]

        let prefix =
            if acceptCoBranded then
                [ prefixDankort; prefixVisaCobranded ].[Cardizer.next 2]
            else
                prefixDankort

        Cardizer.GenerateCard prefix 16

    /// <summary>Returns a random InterPayment number.</summary>
    /// <returns>Random InterPayment number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextInterPayment"/> method.
    /// <code>
    /// void PrintInterPayment()
    /// {
    ///    Console.WriteLine(Cardizer.NextInterPayment());
    /// }
    /// </code>
    /// </example>
    static member NextInterPayment([<Optional; DefaultParameterValue(From16To19.Random)>] interPaymentLengthOption) =
        let length =
            match interPaymentLengthOption with
            | From16To19.Random -> Cardizer.NextInRange 16 19
            | _ -> int interPaymentLengthOption

        Cardizer.GenerateCard [ 6; 3; 6 ] length

    /// <summary>Returns a random UnionPay number.</summary>
    /// <returns>Random UnionPay number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextUnionPay"/> method.
    /// <code>
    /// void PrintUnionPay()
    /// {
    ///    Console.WriteLine(Cardizer.NextUnionPay());
    /// }
    /// </code>
    /// </example>
    static member NextUnionPay([<Optional; DefaultParameterValue(From16To19.Random)>] unionPayLengthOption) =
        let length =
            match unionPayLengthOption with
            | From16To19.Random -> Cardizer.NextInRange 16 19
            | _ -> int unionPayLengthOption

        Cardizer.GenerateCard [ 6; 2 ] length

    /// <summary>Returns a random Tunion number.</summary>
    /// <returns>Random Tunion number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextTunion"/> method.
    /// <code>
    /// void PrintTunion()
    /// {
    ///    Console.WriteLine(Cardizer.NextTunion());
    /// }
    /// </code>
    /// </example>
    static member NextTunion() = Cardizer.GenerateCard [ 3; 1 ] 19

    /// <summary>Returns a random LankaPay number.</summary>
    /// <returns>Random LankaPay number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextLankaPay"/> method.
    /// <code>
    /// void PrintLankaPay()
    /// {
    ///    Console.WriteLine(Cardizer.NextLankaPay());
    /// }
    /// </code>
    /// </example>
    static member NextLankaPay() =
        let prefix = [ 3; 5; 7; 1; 1; 1 ]
        Cardizer.GenerateCard prefix 16

    /// <summary>Returns a random Laser number.</summary>
    /// <returns>Random Laser number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextLaser"/> method.
    /// <code>
    /// void PrintLaser()
    /// {
    ///    Console.WriteLine(Cardizer.NextLaser());
    /// }
    /// </code>
    /// </example>
    static member NextLaser([<Optional; DefaultParameterValue(From16To19.Random)>] laserLengthOption) =
        let length =
            match laserLengthOption with
            | From16To19.Random -> Cardizer.NextInRange 16 19
            | _ -> int laserLengthOption

        let prefix =
            [ [ 6; 3; 0; 4 ]
              [ 6; 7; 0; 6 ]
              [ 6; 7; 7; 1 ]
              [ 6; 7; 0; 9 ] ].[Cardizer.next 4]

        Cardizer.GenerateCard prefix length

    /// <summary>Returns a random InstaPayment number.</summary>
    /// <returns>Random InstaPayment number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextInstaPayment"/> method.
    /// <code>
    /// void PrintInstaPayment()
    /// {
    ///     Console.WriteLine(Cardizer.NextInstaPayment());
    /// }
    /// </code>
    /// </example>
    static member NextInstaPayment() =
        let prefix =
            [ [ 6; 3; 7 ]
              [ 6; 3; 8 ]
              [ 6; 3; 9 ] ].[Cardizer.next 3]

        Cardizer.GenerateCard prefix 16

    /// <summary>Returns a random Visa Electron number.</summary>
    /// <returns>Random Visa Electron number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextVisaElectron"/> method.
    /// <code>
    /// void PrintVisaElectron()
    /// {
    ///     Console.WriteLine(Cardizer.NextVisaElectron());
    /// }
    /// </code>
    /// </example>
    static member NextVisaElectron() =
        let prefix =
            [ [ 4; 0; 2; 6 ]
              [ 4; 1; 7; 5; 0; 0; ]
              [ 4; 5; 0; 8 ]
              [ 4; 8; 4; 4 ]
              [ 4; 9; 1; 3 ]
              [ 4; 9; 1; 7 ] ].[Cardizer.next 6]

        Cardizer.GenerateCard prefix 16
 
    /// <summary>Returns a random Troy number.</summary>
    /// <returns>Random Troy number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextTroy"/> method.
    /// <code>
    /// void PrintTroy()
    /// {
    ///     Console.WriteLine(Cardizer.NextTroy());
    /// }
    /// </code>
    /// </example>
    static member NextTroy() =
        let prefix =
            [ [ 6; 5 ]
              [ 9; 7; 9; 2 ] ].[Cardizer.next 2]

        Cardizer.GenerateCard prefix 16

    /// <summary>Returns a random Solo number.</summary>
    /// <returns>Random Solo number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextSolo"/> method.
    /// <code>
    /// void PrintSolo()
    /// {
    ///    Console.WriteLine(Cardizer.NextSolo()); // randomized between 16, 18 or 19
    ///    Console.WriteLine(Cardizer.NextSolo(From16To19Skip17.Random)); // randomized between 16, 18 or 19
    ///    Console.WriteLine(Cardizer.NextSolo(From16To19Skip17.Sixteen));
    /// }
    /// </code>
    /// </example>
    static member NextSolo([<Optional; DefaultParameterValue(From16To19Skip17.Random)>] soloLengthOption) =
        let length =
            match soloLengthOption with
            | From16To19Skip17.Random -> [ 16; 18; 19 ].[Cardizer.next 3]
            | _ -> int soloLengthOption

        let prefix =
            [ [ 6; 3; 3; 4 ]
              [ 6; 7; 6; 7 ] ].[Cardizer.next 2]

        Cardizer.GenerateCard prefix length
