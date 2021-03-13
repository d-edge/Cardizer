module Tests

open Xunit

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
let ``Should generate valid Visa`` () =
    let visa = Dedge.Cardizer.generateVisa ()
    Assert.StartsWith("4", visa)
    Assert.True(luhn visa)

[<Fact>]
let ``Should generate valid MasterCard`` () =
    let masterCard = Dedge.Cardizer.generateMasterCard ()
    Assert.StartsWith("5", masterCard)
    Assert.True(luhn masterCard)