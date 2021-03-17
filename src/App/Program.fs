open System

[<EntryPoint>]
let main _ =
    Dedge.Cardizer.generateVisa () |> printfn "Visa:\t\t%s"
    Dedge.Cardizer.generateDiscover () |> printfn "Discover:\t%s"
    Dedge.Cardizer.generateMasterCard () |> printfn "MasterCard:\t%s"
    0