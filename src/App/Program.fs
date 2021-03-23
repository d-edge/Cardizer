open Dedge

[<EntryPoint>]
let main _ =
    Cardizer.NextAmex () |> printfn "Amex:\t\t%s"
    Cardizer.NextDiscover () |> printfn "Discover:\t%s"
    Cardizer.NextJcb () |> printfn "Jcb:\t\t%s"
    Cardizer.NextMasterCard () |> printfn "MasterCard:\t%s"
    Cardizer.NextMir () |> printfn "Mir:\t\t%s"
    Cardizer.NextVerve () |> printfn "Verve:\t\t%s"
    Cardizer.NextVisa () |> printfn "Visa:\t\t%s"
    0
