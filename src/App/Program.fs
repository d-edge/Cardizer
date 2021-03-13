open System

[<EntryPoint>]
let main _ =
    Dedge.Cardizer.generateVisa () |> printfn "Visa:\t\t%s"
    Dedge.Cardizer.generateAmex () |> printfn "Amex:\t\t%s"
    0