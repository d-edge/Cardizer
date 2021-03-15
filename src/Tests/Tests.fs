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
let ``Should generate valid Verve`` () =
    let card = Dedge.Cardizer.generateVerve ()
    let start = card.Substring(0, 6) |> int

    let prefixInRange =
        start >= 506099 && start <= 506198
        || start >= 650002 && start <= 650027

    Assert.True(prefixInRange, $"The credit card should not start with {start}.")
    Assert.True(card.Length = 16 || card.Length = 19, $"The credit card should have a length of 16 or 19 but has {card.Length}.")
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")
