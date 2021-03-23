module Tests

open Xunit
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

[<Theory>]
[<InlineData(VisaLengthOptions.Thirteen, 13, 13)>]
[<InlineData(VisaLengthOptions.Sixteen, 16, 16)>]
[<InlineData(VisaLengthOptions.Random, 13, 16)>]
let ``Should generate valid Visa`` length a b =
    let card = Cardizer.NextVisa length

    Assert.StartsWith("4", card)
    Assert.True(card.Length = a || card.Length = b, $"The credit card should have a length of 16 but has {card.Length}.")
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")

[<Theory>]
[<InlineData(VerveLengthOptions.Sixteen, 16, 16)>]
[<InlineData(VerveLengthOptions.Nineteen, 19, 19)>]
[<InlineData(VerveLengthOptions.Random, 16, 19)>]
let ``Should generate valid Verve`` length a b =
    let card = Cardizer.NextVerve length
    let start = card.Substring(0, 6) |> int

    let prefixInRange =
        start >= 506099 && start <= 506198
        || start >= 650002 && start <= 650027

    Assert.True(prefixInRange, $"The credit card should not start with {start}.")

    Assert.True(
        card.Length = a || card.Length = b,
        $"The credit card should have a length of 16 or 19 but has {card.Length}."
    )

    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")

[<Theory>]
[<InlineData(MirLengthOptions.Sixteen, 16, 16)>]
[<InlineData(MirLengthOptions.Seventeen, 17, 17)>]
[<InlineData(MirLengthOptions.Eightteen, 18, 18)>]
[<InlineData(MirLengthOptions.Nineteen, 19, 19)>]
[<InlineData(MirLengthOptions.Random, 16, 19)>]
let ``Should generate valid Mir`` length low high =
    let card = Cardizer.NextMir length
    Assert.StartsWith("220", card)
    Assert.InRange(card.[3], '0', '4')
    Assert.InRange(card.Length, low, high)
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")

[<Theory>]
[<InlineData(JcbLengthOptions.Sixteen, 16, 16)>]
[<InlineData(JcbLengthOptions.Seventeen, 17, 17)>]
[<InlineData(JcbLengthOptions.Eightteen, 18, 18)>]
[<InlineData(JcbLengthOptions.Nineteen, 19, 19)>]
[<InlineData(JcbLengthOptions.Random, 16, 19)>]
let ``Should generate valid Jcb`` length low high =
    let card = Cardizer.NextJcb length
    Assert.StartsWith("35", card)
    Assert.InRange(charToInt card.[2], 2, 8)
    Assert.InRange(charToInt card.[3], 8, 9)
    Assert.InRange(card.Length, low, high)
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")

[<Fact>]
let ``Should generate valid Amex`` () =
    let card = Cardizer.NextAmex()
    Assert.StartsWith("3", card)
    Assert.Contains(string card.[1], "47")
    Assert.Equal(15, card.Length)
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")

[<Theory>]
[<InlineData(DiscoverLengthOptions.Sixteen, 16, 16)>]
[<InlineData(DiscoverLengthOptions.Seventeen, 17, 17)>]
[<InlineData(DiscoverLengthOptions.Eightteen, 18, 18)>]
[<InlineData(DiscoverLengthOptions.Nineteen, 19, 19)>]
[<InlineData(DiscoverLengthOptions.Random, 16, 19)>]
let ``Should generate valid Discover`` length low high =
    let card = Cardizer.NextDiscover length
    Assert.StartsWith("6011", card)
    Assert.InRange(card.Length, low, high)
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")

[<Fact>]
let ``Should generate valid MasterCard`` () =
    let card = Cardizer.NextMasterCard()
    Assert.StartsWith("5", card)
    Assert.Equal(16, card.Length)
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")
