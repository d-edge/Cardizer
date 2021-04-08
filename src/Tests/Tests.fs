module Tests

open Xunit
// note: we may want to move to expecto instead
open FsUnit
open FsUnit.Xunit
open Dedge

// https://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#F.23
let luhn (s: string) =
    let rec g r c =
        function
        | 0 -> r
        | i ->
            let d = ((int s.[i - 1]) - 48) <<< c
            g (r + if d < 10 then d else d - 9) (1 - c) (i - 1)

    (g 0 0 s.Length) % 10 = 0

[<Fact>]
let ``Should generate valid Visa 13`` () =
    let card =
        Cardizer.NextVisa VisaLengthOptions.Thirteen

    card |> should startWith "4"
    card |> should haveLength 13
    card |> luhn |> should be True

[<Fact>]
let ``Should generate valid Visa 16`` () =
    let card =
        Cardizer.NextVisa VisaLengthOptions.Sixteen

    card |> should startWith "4"
    card |> should haveLength 16
    card |> luhn |> should be True

[<Fact>]
let ``Should generate valid Verve`` () =
    let card = Cardizer.NextVerve()
    let start = card.Substring(0, 6) |> int

    let prefixInRange =
        start >= 506099 && start <= 506198
        || start >= 650002 && start <= 650027

    prefixInRange |> should be True
    [ card.Length ] |> should be (subsetOf [ 16; 19 ]) // note: is there a better way for a is b or c?
    card |> luhn |> should be True

[<Fact>]
let ``Should generate valid Mir`` () =
    let card = Cardizer.NextMir()

    card |> should startWith "220"
    card.[3] |> should be (inRange '0' '4')
    card |> should haveLength 16
    card |> luhn |> should be True

[<Fact>]
let ``Should generate valid Jcb`` () =
    let card = Cardizer.NextJcb()

    card |> should startWith "35"
    card.[2] |> should be (inRange '2' '8')
    card.[3] |> should be (inRange '8' '9')
    card |> should haveLength 16
    card |> luhn |> should be True

[<Fact>]
let ``Should generate valid Amex`` () =
    let card = Cardizer.NextAmex()

    card |> should startWith "3"
    [ card.[1] ] |> should be (subsetOf [ '4'; '7' ]) // note: is there a better way for a is b or c?
    card |> should haveLength 15
    card |> luhn |> should be True

[<Fact>]
let ``Should generate valid Discover`` () =
    let card = Cardizer.NextDiscover()
    
    card |> should startWith "6011"
    card |> should haveLength 16
    card |> luhn |> should be True

[<Fact>]
let ``Should generate valid MasterCard`` () =
    let card = Cardizer.NextMasterCard()

    card |> should startWith "5"
    card |> should haveLength 16
    card |> luhn |> should be True
