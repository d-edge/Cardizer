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
    let card = Dedge.Cardizer.generateVisa ()
    Assert.StartsWith("4", card)
    Assert.Equal(16, card.Length)
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")

[<Fact>]
let ``Should generate valid Jcb`` () =
    let card = Dedge.Cardizer.generateJcb ()
    Assert.StartsWith("35", card)
    Assert.Contains(string card.[2], "2345678")
    Assert.Contains(string card.[3], "89")
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")
