open DEdge

[<EntryPoint>]
let main _ =
    let cardizer = new Cardizer()
    cardizer.NextAmex() |> printfn "Amex:\t\t%s"
    cardizer.NextDiscover() |> printfn "Discover:\t%s"
    cardizer.NextJcb() |> printfn "Jcb:\t\t%s"
    cardizer.NextMasterCard() |> printfn "MasterCard:\t%s"
    cardizer.NextMir() |> printfn "Mir:\t\t%s"
    cardizer.NextVerve() |> printfn "Verve:\t\t%s"
    cardizer.NextVisa() |> printfn "Visa:\t\t%s"
    0
