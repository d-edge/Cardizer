open System

[<EntryPoint>]
let main _ =
    Dedge.Cardizer.generateVisa () |> printfn "Visa:\t\t%s"
    Dedge.Cardizer.generateJcb () |> printfn "Jcb:\t\t%s"
    0 // return an integer exit code