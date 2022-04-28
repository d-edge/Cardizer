namespace DEdge

open System
open System.Runtime.InteropServices

type VisaLengthOptions =
    | Random = 0
    | Thirteen = 13
    | Sixteen = 16

type From12To19 =
    | Random = 0
    | Twelve = 12
    | Thirteen = 13
    | Fourteen = 14
    | Fifteen = 15
    | Sixteen = 16
    | Seventeen = 17
    | Eighteen = 18
    | Nineteen = 19

type From16To19 =
    | Random = 0
    | Sixteen = 16
    | Seventeen = 17
    | Eighteen = 18
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
    | Eighteen = 18
    | Nineteen = 19

type Cardizer(random:IRandom) =
    new() =
        Cardizer(ThreadLocalRandom())

    new(seed: int) =
        Cardizer(ThreadLocalRandom(seed))

    new(random: Random) =
        Cardizer(ThreadLocalRandom(random))

    /// <summary>Returns a random integer within a given range.</summary>
    /// <param name="low">The (inclusive) low value of the range</param>
    /// <param name="high">The (inclusive) high value of the range</param>
    /// <returns>Random integer within a given range</returns>
    member private this.NextInRange low high = random.Next (high - low + 1) + low

    /// <summary>Returns a sequence of each digit of a given number.</summary>
    /// <param name="number">The number to enumerate</param>
    /// <returns>A sequence of each digit of a given number</returns>
    member private _.NumberToSeq number =
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
    member private this.NextSeqInRange low high =
        this.NextInRange low high
        |> this.NumberToSeq

    member private getEnumValues<'T> () = (System.Enum.GetValues(typeof<'T>) :?> (int [])) |> Array.toList

    member private filterPositive l = l |> List.filter(fun x -> x > 0)

    member private nextEnumValue<'T> () =
        let values = getEnumValues<'T>() |> filterNonZero
        let length = List.length values
        values.[random.Next(length)]

    member private _.GetNumber n =
        let n2 = n * 2
        if n2 > 9 then n2 - 9 else n2

    member inline private _.CheckDigit sum = ((sum / 10 + 1) * 10 - sum) % 10

    member private this.NextUniquePersonalIdentifiers n =
        { 1 .. n } |> Seq.map (fun _ -> random.Next 10)

    member private this.ReverseSum(digits: seq<int>) : int =
        digits
        |> Seq.rev
        |> Seq.mapi (fun i n -> i % 2 = 0, n)
        |> Seq.sumBy
            (fun (isEven, n) ->
                if isEven then
                    this.GetNumber n
                else
                    n)
        |> this.CheckDigit

    member private this.AppendSum digits =
        seq {
            yield! digits
            yield this.ReverseSum digits
        }

    member private this.GenerateCard (prefixes: seq<int>) cardLen =
        let checksumLen = 1
        let prefixesLen = Seq.length prefixes
        let generateLen = cardLen - prefixesLen - checksumLen

        generateLen
        |> this.NextUniquePersonalIdentifiers
        |> Seq.toList
        |> Seq.append prefixes
        |> this.AppendSum
        |> String.Concat

    /// <summary>Returns a random Visa number that is of the given available length.</summary>
    /// <param name="visaLengthOption">Credit card's length (default is randomized between 13 or 16)</param>
    /// <returns>Random Visa number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextVisa"/> method.
    /// <code>
    /// void PrintVisa()
    /// {
    ///    Console.WriteLine(this.NextVisa()); // randomized between 13 or 16
    ///    Console.WriteLine(this.NextVisa(VisaLengthOptions.Random)); // randomized between 13 or 16
    ///    Console.WriteLine(this.NextVisa(VisaLengthOptions.Sixteen));
    /// }
    /// </code>
    /// </example>
    member this.NextVisa([<Optional; DefaultParameterValue(VisaLengthOptions.Random)>] visaLengthOption) =
        let length =
            match visaLengthOption with
            | VisaLengthOptions.Random -> if random.Next 2 = 0 then 13 else 16
            | _ -> int visaLengthOption


        this.GenerateCard [ 4 ] length

    /// <summary>Returns a random Verve number that is of the given available length.</summary>
    /// <param name="From16To19Skip17">Credit card's length (default is randomized between 16, 18 or 19)</param>
    /// <returns>Random Verve number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextVerve"/> method.
    /// <code>
    /// void PrintVerve()
    /// {
    ///    Console.WriteLine(this.NextVerve()); // randomized between 16, 18 or 19
    ///    Console.WriteLine(this.NextVerve(From16To19Skip17.Random)); // randomized between 16, 18 or 19
    ///    Console.WriteLine(this.NextVerve(From16To19Skip17.Sixteen));
    /// }
    /// </code>
    /// </example>
    member this.NextVerve([<Optional; DefaultParameterValue(From16To19Skip17.Random)>] verveLengthOption) =
        let length =
            match verveLengthOption with
            | From16To19Skip17.Random -> nextEnumValue<From16To19Skip17>()
            | _ -> int verveLengthOption

        let prefix =
            [ [ 506099; 506198 ]
              [ 650002; 650027 ]
              [ 507865; 507964 ] ].[random.Next 3]

        let prefixes =
            this.NextInRange prefix.[0] prefix.[1]
            |> this.NumberToSeq

        this.GenerateCard prefixes length

    /// <summary>Returns a random Mir number that is of the given available length.</summary>
    /// <param name="mirLengthOption">Credit card's length (default is randomized between 16 and 19)</param>
    /// <returns>Random Mir number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextMir"/> method.
    /// <code>
    /// void PrintMir()
    /// {
    ///    Console.WriteLine(this.NextMir()); // randomized between 16 and 19
    ///    Console.WriteLine(this.NextMir(MirLengthOptions.Random)); // randomized between 16 and 19
    ///    Console.WriteLine(this.NextMir(MirLengthOptions.Sixteen));
    /// }
    /// </code>
    /// </example>
    member this.NextMir([<Optional; DefaultParameterValue(From16To19.Random)>] mirLengthOption) =
        let length =
            match mirLengthOption with
            | From16To19.Random -> this.NextInRange 16 19
            | _ -> int mirLengthOption

        let prefixes = [ 2; 2; 0; random.Next 5 ]

        this.GenerateCard prefixes length

    /// <summary>Returns a random Jcb number that is of the given available length.</summary>
    /// <param name="jcbLengthOption">Credit card's length (default is randomized between 16 and 19)</param>
    /// <returns>Random Jcb number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextJcb"/> method.
    /// <code>
    /// void PrintJcb()
    /// {
    ///    Console.WriteLine(this.NextJcb()); // randomized between 16 and 19
    ///    Console.WriteLine(this.NextJcb(JcbLengthOptions.Random)); // randomized between 16 and 19
    ///    Console.WriteLine(this.NextJcb(JcbLengthOptions.Sixteen));
    /// }
    /// </code>
    /// </example>
    member this.NextJcb([<Optional; DefaultParameterValue(From16To19.Random)>] jcbLengthOption) =
        let length =
            match jcbLengthOption with
            | From16To19.Random -> this.NextInRange 16 19
            | _ -> int jcbLengthOption

        let prefixes =
            [ 3
              5
              this.NextInRange 2 8
              this.NextInRange 8 9 ]

        this.GenerateCard prefixes length

    /// <summary>Returns a random Amex number.</summary>
    /// <returns>Random Amex number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextAmex"/> method.
    /// <code>
    /// void PrintAmex()
    /// {
    ///    Console.WriteLine(this.NextAmex());
    /// }
    /// </code>
    /// </example>
    member this.NextAmex() =
        let second = if random.Next 2 = 0 then 4 else 7
        this.GenerateCard [ 3; second ] 15

    /// <summary>Returns a random Discover number.</summary>
    /// <returns>Random Discover number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDiscover"/> method.
    /// <code>
    /// void PrintDiscover()
    /// {
    ///    Console.WriteLine(this.NextDiscover());
    /// }
    /// </code>
    /// </example>
    member this.NextDiscover([<Optional; DefaultParameterValue(From16To19.Random)>] discoverLengthOption, [<Optional; DefaultParameterValue(true)>] acceptCoBranded: bool) =
        let length =
            match discoverLengthOption with
            | From16To19.Random -> this.NextInRange 16 19
            | _ -> int discoverLengthOption

        let roll = random.Next (if acceptCoBranded then 4 else 3)
        let prefix =
            match roll with
            | 0 -> [ 6; 0; 1; 1 ]
            | 1 -> [ 6; 5 ]
            | 2 -> this.NextSeqInRange 644 649
            | _ -> this.NextSeqInRange 622126 622925
        
        this.GenerateCard prefix length

    /// <summary>Returns a random MasterCard number.</summary>
    /// <returns>Random MasterCard number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextMasterCard"/> method.
    /// <code>
    /// void PrintMasterCard()
    /// {
    ///    Console.WriteLine(this.NextMasterCard());
    /// }
    /// </code>
    /// </example>
    member this.NextMasterCard() =
        let prefixes =
            if random.Next 2 = 0 then
                this.NextSeqInRange 51 55
            else
                this.NextSeqInRange 2221 2720

        this.GenerateCard prefixes 16


    /// <summary>Returns a random Uatp number.</summary>
    /// <returns>Random Uatp number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextUatp"/> method.
    /// <code>
    /// void PrintUatp()
    /// {
    ///    Console.WriteLine(this.NextUatp());
    /// }
    /// </code>
    /// </example>
    member this.NextUatp() = this.GenerateCard [ 1 ] 15

    /// <summary>Returns a random RuPay number.</summary>
    /// <returns>Random RuPay number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextRuPay"/> method.
    /// <code>
    /// void PrintRuPay()
    /// {
    ///    Console.WriteLine(this.NextRuPay());
    /// }
    /// </code>
    /// </example>
    member this.NextRuPay([<Optional; DefaultParameterValue(true)>] acceptCoBranded: bool) =
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
                  prefixRuPayAndJcbCobranded ].[random.Next 2]
  
            if merge.Length = 2 then
                this.GenerateCard merge.[random.Next 2] 16
            else
                this.GenerateCard merge.[random.Next 5] 16
        else
            this.GenerateCard prefixRuPay.[random.Next 5] 16

    /// <summary>Returns a random DinersClubInternational number.</summary>
    /// <returns>Random DinersClubInternational number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDinersClubInternational"/> method.
    /// <code>
    /// void PrintDinersClubInternational()
    /// {
    ///    Console.WriteLine(this.NextDinersClubInternational());
    /// }
    /// </code>
    /// </example>
    member this.NextDinersClubInternational
        ([<Optional; DefaultParameterValue(DinersClubInternationalLengthOptions.Random)>] dinersLengthOption)
        =
        let length =
            match dinersLengthOption with
            | DinersClubInternationalLengthOptions.Random -> this.NextInRange 14 19
            | _ -> int dinersLengthOption

        this.GenerateCard [ 3; 6 ] length

    /// <summary>Returns a random DinersClubUsAndCanada number.</summary>
    /// <returns>Random DinersClubUsAndCanada number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDDinersClubUsAndCanada"/> method.
    /// <code>
    /// void PrintDinersClubUsAndCanada()
    /// {
    ///    Console.WriteLine(this.NextDinersClubUsAndCanada());
    /// }
    /// </code>
    /// </example>
    member this.NextDinersClubUsAndCanada() = this.GenerateCard [ 5; 4 ] 16

    /// <summary>Returns a random DinersClubInternational or DinersClubUsAndCanada number.</summary>
    /// <returns>Random DinersClubInternational or DinersClubUsAndCanada number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDinersClub"/> method.
    /// <code>
    /// void PrintDinersClub()
    /// {
    ///    Console.WriteLine(this.NextDinersClub());
    /// }
    /// </code>
    /// </example>
    member this.NextDinersClub() =
        if random.Next 2 = 0 then
            this.NextDinersClubUsAndCanada()
        else
            this.NextDinersClubInternational()

    /// <summary>Returns a random Maestro number.</summary>
    /// <returns>Random Maestro number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextMaestro"/> method.
    /// <code>
    /// void PrintMaestro()
    /// {
    ///    Console.WriteLine(this.NextMaestro());
    /// }
    /// </code>
    /// </example>
    member this.NextMaestro([<Optional; DefaultParameterValue(From12To19.Random)>] maestroLengthOption) =
        let length =
            match maestroLengthOption with
            | From12To19.Random -> this.NextInRange 12 19
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
              [ 6; 7; 6; 3 ] ].[random.Next 9]

        this.GenerateCard prefix length

    /// <summary>Returns a random Dankort number.</summary>
    /// <returns>Random Dankort number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextDankort"/> method.
    /// <code>
    /// void PrintDankort()
    /// {
    ///    Console.WriteLine(this.NextDankort());
    /// }
    /// </code>
    /// </example>
    member this.NextDankort([<Optional; DefaultParameterValue(true)>] acceptCoBranded: bool) =
        let prefixDankort = [ 5; 0; 1; 9 ]
        let prefixVisaCobranded = [ 4; 5; 7; 1 ]

        let prefix =
            if acceptCoBranded then
                [ prefixDankort; prefixVisaCobranded ].[random.Next 2]
            else
                prefixDankort

        this.GenerateCard prefix 16

    /// <summary>Returns a random InterPayment number.</summary>
    /// <returns>Random InterPayment number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextInterPayment"/> method.
    /// <code>
    /// void PrintInterPayment()
    /// {
    ///    Console.WriteLine(this.NextInterPayment());
    /// }
    /// </code>
    /// </example>
    member this.NextInterPayment([<Optional; DefaultParameterValue(From16To19.Random)>] interPaymentLengthOption) =
        let length =
            match interPaymentLengthOption with
            | From16To19.Random -> this.NextInRange 16 19
            | _ -> int interPaymentLengthOption

        this.GenerateCard [ 6; 3; 6 ] length

    /// <summary>Returns a random UnionPay number.</summary>
    /// <returns>Random UnionPay number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextUnionPay"/> method.
    /// <code>
    /// void PrintUnionPay()
    /// {
    ///    Console.WriteLine(this.NextUnionPay());
    /// }
    /// </code>
    /// </example>
    member this.NextUnionPay([<Optional; DefaultParameterValue(From16To19.Random)>] unionPayLengthOption) =
        let length =
            match unionPayLengthOption with
            | From16To19.Random -> this.NextInRange 16 19
            | _ -> int unionPayLengthOption

        this.GenerateCard [ 6; 2 ] length

    /// <summary>Returns a random Tunion number.</summary>
    /// <returns>Random Tunion number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextTunion"/> method.
    /// <code>
    /// void PrintTunion()
    /// {
    ///    Console.WriteLine(this.NextTunion());
    /// }
    /// </code>
    /// </example>
    member this.NextTunion() = this.GenerateCard [ 3; 1 ] 19

    /// <summary>Returns a random LankaPay number.</summary>
    /// <returns>Random LankaPay number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextLankaPay"/> method.
    /// <code>
    /// void PrintLankaPay()
    /// {
    ///    Console.WriteLine(this.NextLankaPay());
    /// }
    /// </code>
    /// </example>
    member this.NextLankaPay() =
        let prefix = [ 3; 5; 7; 1; 1; 1 ]
        this.GenerateCard prefix 16

    /// <summary>Returns a random Laser number.</summary>
    /// <returns>Random Laser number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextLaser"/> method.
    /// <code>
    /// void PrintLaser()
    /// {
    ///    Console.WriteLine(this.NextLaser());
    /// }
    /// </code>
    /// </example>
    member this.NextLaser([<Optional; DefaultParameterValue(From16To19.Random)>] laserLengthOption) =
        let length =
            match laserLengthOption with
            | From16To19.Random -> this.NextInRange 16 19
            | _ -> int laserLengthOption

        let prefix =
            [ [ 6; 3; 0; 4 ]
              [ 6; 7; 0; 6 ]
              [ 6; 7; 7; 1 ]
              [ 6; 7; 0; 9 ] ].[random.Next 4]

        this.GenerateCard prefix length

    /// <summary>Returns a random InstaPayment number.</summary>
    /// <returns>Random InstaPayment number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextInstaPayment"/> method.
    /// <code>
    /// void PrintInstaPayment()
    /// {
    ///     Console.WriteLine(this.NextInstaPayment());
    /// }
    /// </code>
    /// </example>
    member this.NextInstaPayment() =
        let prefix =
            [ [ 6; 3; 7 ]
              [ 6; 3; 8 ]
              [ 6; 3; 9 ] ].[random.Next 3]

        this.GenerateCard prefix 16

    /// <summary>Returns a random Switch number.</summary>
    /// <returns>Random Switch number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextSwitch"/> method.
    /// <code>
    /// void PrintSwitch()
    /// {
    ///    Console.WriteLine(this.NextSwitch());
    ///    Console.WriteLine(this.NextSwitch()); // randomized between 16, 18 or 19
    ///    Console.WriteLine(this.NextSwitch(From16To19Skip17.Random)); // randomized between 16, 18 or 19
    ///    Console.WriteLine(this.NextSwitch(From16To19Skip17.Sixteen));
    /// }
    /// </code>
    /// </example>
    member this.NextSwitch([<Optional; DefaultParameterValue(From16To19Skip17.Random)>] switchLengthOption) =
        let length =
            match switchLengthOption with
            | From16To19Skip17.Random -> [ 16; 18; 19 ].[random.Next 3]
            | _ -> int switchLengthOption

        let prefix =
            [ [ 4; 9; 0; 3 ]
              [ 4; 9; 0; 5 ]
              [ 4; 9; 1; 1 ]
              [ 4; 9; 3; 6 ]
              [ 5; 6; 4; 1; 8; 2 ]
              [ 6; 3; 3; 1; 1; 0 ]
              [ 6; 3; 3; 3 ]
              [ 6; 7; 5; 9 ] ].[random.Next 8]

        this.GenerateCard prefix length

    /// <summary>Returns a random Visa Electron number.</summary>
    /// <returns>Random Visa Electron number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextVisaElectron"/> method.
    /// <code>
    /// void PrintVisaElectron()
    /// {
    ///     Console.WriteLine(this.NextVisaElectron());
    /// }
    /// </code>
    /// </example>
    member this.NextVisaElectron() =
        let prefix =
            [ [ 4; 0; 2; 6 ]
              [ 4; 1; 7; 5; 0; 0; ]
              [ 4; 5; 0; 8 ]
              [ 4; 8; 4; 4 ]
              [ 4; 9; 1; 3 ]
              [ 4; 9; 1; 7 ] ].[random.Next 6]

        this.GenerateCard prefix 16
 
    /// <summary>Returns a random Troy number.</summary>
    /// <returns>Random Troy number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextTroy"/> method.
    /// <code>
    /// void PrintTroy()
    /// {
    ///     Console.WriteLine(this.NextTroy());
    /// }
    /// </code>
    /// </example>
    member this.NextTroy() =
        let prefix =
            [ [ 6; 5 ]
              [ 9; 7; 9; 2 ] ].[random.Next 2]

        this.GenerateCard prefix 16

    /// <summary>Returns a random Solo number.</summary>
    /// <returns>Random Solo number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextSolo"/> method.
    /// <code>
    /// void PrintSolo()
    /// {
    ///    Console.WriteLine(this.NextSolo()); // randomized between 16, 18 or 19
    ///    Console.WriteLine(this.NextSolo(From16To19Skip17.Random)); // randomized between 16, 18 or 19
    ///    Console.WriteLine(this.NextSolo(From16To19Skip17.Sixteen));
    /// }
    /// </code>
    /// </example>
    member this.NextSolo([<Optional; DefaultParameterValue(From16To19Skip17.Random)>] soloLengthOption) =
        let length =
            match soloLengthOption with
            | From16To19Skip17.Random -> [ 16; 18; 19 ].[random.Next 3]
            | _ -> int soloLengthOption

        let prefix =
            [ [ 6; 3; 3; 4 ]
              [ 6; 7; 6; 7 ] ].[random.Next 2]

        this.GenerateCard prefix length

    /// <summary>Returns a random UzCard number.</summary>
    /// <returns>Random UzCard number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextUzCard"/> method.
    /// <code>
    /// void PrintUzCard()
    /// {
    ///    Console.WriteLine(this.NextUzCard());
    /// }
    /// </code>
    /// </example>
    member this.NextUzCard() =
        let prefix = [ 8; 6; 0; 0 ]
        this.GenerateCard prefix 16

    /// <summary>Returns a random Humo number.</summary>
    /// <returns>Random Humo number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextHumo"/> method.
    /// <code>
    /// void PrintHumo()
    /// {
    ///    Console.WriteLine(this.NextHumo());
    /// }
    /// </code>
    /// </example>
    member this.NextHumo() =
        let prefix = [ 9; 8; 6; 0 ]
        this.GenerateCard prefix 16

    // <summary>Returns a random NPS Pridnestrovie number.</summary>
    /// <returns>Random NPS Pridnestrovie number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextNPSPridnestrovie"/> method.
    /// <code>
    /// void PrintNPSPridnestrovie()
    /// {
    ///    Console.WriteLine(this.NextNPSPridnestrovie()); 
    /// }
    /// </code>
    /// </example>
    member this.NextNPSPridnestrovie() =
        let prefix = this.NextSeqInRange 6054740 6054744
        this.GenerateCard prefix 16

    /// <summary>Returns a random Maestro UK number.</summary>
    /// <returns>Random Maestro UK number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextMaestroUK"/> method.
    /// <code>
    /// void PrintMaestroUK()
    /// {
    ///    Console.WriteLine(this.NextMaestroUK()); 
    /// }
    /// </code>
    /// </example>
    member this.NextMaestroUK([<Optional; DefaultParameterValue(From12To19.Random)>] maestroUKLengthOption) =
        let length =
            match maestroUKLengthOption with
            | From12To19.Random -> this.NextInRange 12 19
            | _ -> int maestroUKLengthOption

        let prefix =
            [ [ 6; 7; 5; 9 ]
              [ 6; 7; 6; 7; 7; 0 ]
              [ 6; 7; 6; 7; 7; 4 ] ].[random.Next 3]

        this.GenerateCard prefix length 

    /// <summary>Returns a random UkrCard number.</summary>
    /// <returns>Random UkrCard number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextUkrCard"/> method.
    /// <code>
    /// void PrintUkrCard()
    /// {
    ///    Console.WriteLine(this.NextUkrCard()); 
    /// }
    /// </code>
    /// </example>
    member this.NextUkrCard([<Optional; DefaultParameterValue(From16To19.Random)>] ukrCardLengthOption) =
        let length =
            match ukrCardLengthOption with
            | From16To19.Random -> this.NextInRange 16 19
            | _ -> int ukrCardLengthOption

        let prefix = this.NextSeqInRange 60400100 60420099

        this.GenerateCard prefix length

    /// <summary>Returns a random Bankcard number.</summary>
    /// <returns>Random Bankcard number</returns>
    /// <example>
    /// This sample shows how to call the <see cref="NextBankcard"/> method.
    /// <code>
    /// void PrintBankcard()
    /// {
    ///    Console.WriteLine(this.NextBankcard());
    /// }
    /// </code>
    /// </example>
    member this.NextBankcard() =
        let roll = random.Next 2

        let prefix =
            if roll = 0 then
                [ 5; 6; 1; 0 ]
            else
                this.NextSeqInRange 560221 560225

        this.GenerateCard prefix 16
