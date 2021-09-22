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
// [<InlineData(VisaLengthOptions.Random, 13, 16)>]
let ``Should generate valid Visa`` length expectedLength =
    let card = Cardizer.NextVisa length

    card |> should startWith "4"
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(VerveLengthOptions.Sixteen, 16)>]
[<InlineData(VerveLengthOptions.Nineteen, 19)>]
// [<InlineData(VerveLengthOptions.Random, 16, 19)>]
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
[<InlineData(MirLengthOptions.Sixteen, 16)>]
[<InlineData(MirLengthOptions.Seventeen, 17)>]
[<InlineData(MirLengthOptions.Eightteen, 18)>]
[<InlineData(MirLengthOptions.Nineteen, 19)>]
// [<InlineData(MirLengthOptions.Random, 16, 19)>]
let ``Should generate valid Mir`` length expectedLength =
    let card = Cardizer.NextMir length

    card |> should startWith "220"
    card.[3] |> should be (inRange '0' '4')
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Theory>]
[<InlineData(JcbLengthOptions.Sixteen, 16)>]
[<InlineData(JcbLengthOptions.Seventeen, 17)>]
[<InlineData(JcbLengthOptions.Eightteen, 18)>]
[<InlineData(JcbLengthOptions.Nineteen, 19)>]
// [<InlineData(JcbLengthOptions.Random, 16, 19)>]
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
[<InlineData(DiscoverLengthOptions.Sixteen, 16)>]
[<InlineData(DiscoverLengthOptions.Seventeen, 17)>]
[<InlineData(DiscoverLengthOptions.Eightteen, 18)>]
[<InlineData(DiscoverLengthOptions.Nineteen, 19)>]
// [<InlineData(DiscoverLengthOptions.Random, 16, 19)>]
let ``Should generate valid Discover`` length expectedLength =
    let card = Cardizer.NextDiscover length

    card |> should startWith "6011"
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Fact>]
let ``Should generate valid MasterCard`` () =
    let card = Cardizer.NextMasterCard()

    card |> should startWith "5"
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
    let start = card.Substring(0, 2) |> int
    let prefixInRange = start = 36
    prefixInRange |> should be True
    card |> should haveLength expectedLength
    card |> luhn |> should be LuhnCheck

[<Fact>]
let ``Should generate valid DinersClubUsAndCanada``  =
    let card = Cardizer.NextDinersClubUsAndCanada()
    card |> should startWith "54"
    card |> should haveLength 16
    card |> luhn |> should be LuhnCheck


[<Theory>]
[<InlineData(DinersClubInternationalLengthOptions.Fourteen, 14)>]
[<InlineData(DinersClubInternationalLengthOptions.Fifteen, 15)>]
[<InlineData(DinersClubInternationalLengthOptions.Sixteen, 16)>]
[<InlineData(DinersClubInternationalLengthOptions.Seventeen, 17)>]
[<InlineData(DinersClubInternationalLengthOptions.Eightteen, 18)>]
[<InlineData(DinersClubInternationalLengthOptions.Nineteen, 19)>]
let ``Should generate valid Diners`` length expectedLength =
    let card = Cardizer.NextDinersClub()
    let start = card.ToString().Substring(0, 2) |> int
    let prefixInRange = start = 36 || start = 54 
    prefixInRange |> should be True
    card |> luhn |> should be LuhnCheck
