open Dedge

[<EntryPoint>]
let main _ =
    Cardizer.GenerateAmex () |> printfn "Amex:\t\t%s"
    Cardizer.GenerateDiscover () |> printfn "Discover:\t%s"
    Cardizer.GenerateJcb () |> printfn "Jcb:\t\t%s"
    Cardizer.GenerateMasterCard () |> printfn "MasterCard:\t%s"
    Cardizer.GenerateMir () |> printfn "Mir:\t\t%s"
    Cardizer.GenerateVerve () |> printfn "Verve:\t\t%s"
    Cardizer.GenerateVisa () |> printfn "Visa:\t\t%s"
    0
