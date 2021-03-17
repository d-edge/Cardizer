[<EntryPoint>]
let main _ =
    Dedge.Cardizer.generateAmex () |> printfn "Amex:\t\t%s"
    Dedge.Cardizer.generateDiscover () |> printfn "Discover:\t%s"
    Dedge.Cardizer.generateJcb () |> printfn "Jcb:\t\t%s"
    Dedge.Cardizer.generateMasterCard () |> printfn "MasterCard:\t%s"
    Dedge.Cardizer.generateMir () |> printfn "Mir:\t\t%s"
    Dedge.Cardizer.generateVerve () |> printfn "Verve:\t\t%s"
    Dedge.Cardizer.generateVisa () |> printfn "Visa:\t\t%s"
    0
