open System

[<EntryPoint>]
let main argv =
    Cardizer.generateVisa () |> printfn "Visa: %s"
    0 // return an integer exit code