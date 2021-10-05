module Tests

open Xunit
// note: we may want to move to expecto instead
open FsUnit.Xunit
open Dedge

let inline charToInt c = int c - 48

// https://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#F.23
let luhn (s: string) =
    let rec g r c =
        function
        | 0 -> r
        | i ->
            let d = (charToInt s.[i - 1]) <<< c
            g (r + if d < 10 then d else d - 9) (1 - c) (i - 1)

    (g 0 0 s.Length) % 10 = 0

// let TrueWithMessage (message: string) : NHamcrest.IMatcher<obj> =
//     let matcher =
//         new NHamcrest.Core.IsEqualMatcher<obj>(true)

//     matcher.DescribedAs(message)

let LuhnCheck : NHamcrest.IMatcher<obj> =
    let matcher =
        new NHamcrest.Core.IsEqualMatcher<obj>(true)

    matcher.DescribedAs $"Fail the Luhn check."

[<Theory>]
[<InlineData(VisaLengthOptions.Thirteen, 13)>]
[<InlineData(VisaLengthOptions.Sixteen, 16)>]
let ``Should generate valid Visa`` length expectedLength =
    let card = Cardizer.NextVisa length

    card |> should startWith "4"
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(VerveLengthOptions.Sixteen, 16)>]
[<InlineData(VerveLengthOptions.Nineteen, 19)>]
let ``Should generate valid Verve`` length expectedLength =
    let card = Cardizer.NextVerve length
    let start = card.Substring(0, 6) |> int

    let prefixInRange =
        start >= 506099 && start <= 506198
        || start >= 650002 && start <= 650027

    prefixInRange |> should be True
    card |> should haveLength expectedLength // (subsetOf [ 16; 19 ]) // note: is there a better way for a is b or c?
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(From16To19.Sixteen, 16)>]
[<InlineData(From16To19.Seventeen, 17)>]
[<InlineData(From16To19.Eightteen, 18)>]
[<InlineData(From16To19.Nineteen, 19)>]
let ``Should generate valid Mir`` length expectedLength =
    let card = Cardizer.NextMir length

    card |> should startWith "220"
    card.[3] |> should be (inRange '0' '4')
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(From16To19.Sixteen, 16)>]
[<InlineData(From16To19.Seventeen, 17)>]
[<InlineData(From16To19.Eightteen, 18)>]
[<InlineData(From16To19.Nineteen, 19)>]
let ``Should generate valid Jcb`` length expectedLength =
    let card = Cardizer.NextJcb length

    card |> should startWith "35"
    card.[2] |> should be (inRange '2' '8')
    card.[3] |> should be (inRange '8' '9')
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Fact>]
let ``Should generate valid Amex`` () =
    let card = Cardizer.NextAmex()

    card |> should startWith "3"
    [ card.[1] ] |> should be (subsetOf [ '4'; '7' ]) // note: is there a better way for a is b or c?
    card |> should haveLength 15
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(From16To19.Sixteen, 16)>]
[<InlineData(From16To19.Seventeen, 17)>]
[<InlineData(From16To19.Eightteen, 18)>]
[<InlineData(From16To19.Nineteen, 19)>]
let ``Should generate valid Discover`` length expectedLength =
    let card = Cardizer.NextDiscover length

    card |> should startWith "6011"
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Fact>]
let ``Should generate valid MasterCard`` () =
    let card = Cardizer.NextMasterCard()
    let start = card.Substring(0, 4) |> int
    let prefixInRange =
        (start >= 2221 && start <= 2720) || (start >= 5100 && start <= 5599)

    prefixInRange |> should be True
    card |> should haveLength 16
    card |> luhn |> should be LuhnCheck

[<Fact>]
let ``Should generate valid Uatp`` () =
    let card = Cardizer.NextUatp()

    card |> should startWith "1"
    card |> should haveLength 15
    card |> luhn |> should be LuhnCheck


[<Theory>]
[<InlineData(DinersClubInternationalLengthOptions.Fourteen, 14)>]
[<InlineData(DinersClubInternationalLengthOptions.Fifteen, 15)>]
[<InlineData(DinersClubInternationalLengthOptions.Sixteen, 16)>]
[<InlineData(DinersClubInternationalLengthOptions.Seventeen, 17)>]
[<InlineData(DinersClubInternationalLengthOptions.Eightteen, 18)>]
[<InlineData(DinersClubInternationalLengthOptions.Nineteen, 19)>]
let ``Should generate valid DinersClubInternational`` length expectedLength =
    let card = Cardizer.NextDinersClubInternational length
    card |> should startWith "36" 
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(From12To19.Twelve, 12)>]
[<InlineData(From12To19.Thirteen, 13)>]
[<InlineData(From12To19.Fourteen, 14)>]
[<InlineData(From12To19.Fifteen, 15)>]
[<InlineData(From12To19.Sixteen, 16)>]
[<InlineData(From12To19.Seventeen, 17)>]
[<InlineData(From12To19.Eightteen, 18)>]
[<InlineData(From12To19.Nineteen, 19)>]
let ``Should generate valid Maestro`` length expectedLength =
    let card = Cardizer.NextMaestro length
    [ card.[0] ] |> should  be (subsetOf ['5';'6']) 
    let start = card.Substring(0, 4) |> int

    let prefixInRange =
        start = 5018 || start = 5020 || start = 5038 || start = 5893
        || start = 6304 || start = 6759 || start = 6761 || start = 6762
        || start = 6763

    prefixInRange |> should be True
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Fact>]
let ``Should generate valid DinersClubUsAndCanada``  =
    let card = Cardizer.NextDinersClubUsAndCanada()
    card |> should startWith "54"
    card |> should haveLength 16
    card |> luhn |> should be LuhnCheck


[<Fact>]
let ``Should generate valid Diners``  =
    let card = Cardizer.NextDinersClub()
    let start = card.Substring(0, 2)
    let prefixInRange = start = "36" || start = "54" 
    prefixInRange |> should be True

let ``Should generate valid Dankort`` () =
    let card = Cardizer.NextDankort false

    card |> should startWith "5019"
    card |> should haveLength 16
    card |> luhn |> should be LuhnCheck

    let card = Cardizer.NextDankort true
    let start = card.Substring(0, 4) |> int
    let prefix = start = 4571 || start = 5019

    prefix |> should be True
    card |> should haveLength 16
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(From16To19.Sixteen, 16)>]
[<InlineData(From16To19.Seventeen, 17)>]
[<InlineData(From16To19.Eightteen, 18)>]
[<InlineData(From16To19.Nineteen, 19)>]
let ``Should generate valid InterPayment`` length expectedLength =
    let card = Cardizer.NextInterPayment length
    card |> should startWith "636"
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(From16To19.Sixteen, 16)>]
[<InlineData(From16To19.Seventeen, 17)>]
[<InlineData(From16To19.Eightteen, 18)>]
[<InlineData(From16To19.Nineteen, 19)>]
let ``Should generate valid UnionPay`` length expectedLength =
    let card = Cardizer.NextUnionPay length
    card |> should startWith "62"
    card |> should haveLength expectedLength

[<Fact>]
let ``Should generate valid Tunion`` () =
    let card = Cardizer.NextTunion()

    card |> should startWith "31"
    card |> should haveLength 19
    card |> luhn |> should be LuhnCheck

[<Fact>]
let ``Should generate valid LankaPay`` () =
    let card = Cardizer.NextLankaPay()

    card |> should startWith "357111"
    card |> should haveLength 16
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(From16To19.Sixteen, 16)>]
[<InlineData(From16To19.Seventeen, 17)>]
[<InlineData(From16To19.Eightteen, 18)>]
[<InlineData(From16To19.Nineteen, 19)>]
let ``Should generate valid Laser`` length expectedLength =
    let card = Cardizer.NextLaser length

    let start = card.Substring(0, 4) |> int

    let prefixInRange =
        start = 6304 || start = 6706 || start = 6771 || start = 6709

    prefixInRange   |> should be True
    card            |> should haveLength expectedLength 
    card            |> luhn |> should be LuhnCheck

[<Fact>]
let ``Should generate valid InstaPayment`` () =
    let card = Cardizer.NextInstaPayment()

    let start = card.Substring(0, 3) |> int

    let prefixInRange =
        start = 637 || start = 638 || start = 639

    prefixInRange   |> should be True
    card            |> should haveLength 16
    card            |> luhn |> should be LuhnCheck

