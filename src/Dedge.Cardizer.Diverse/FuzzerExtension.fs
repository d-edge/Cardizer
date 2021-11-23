namespace Dedge.Cardizer.Diverse

open Diverse
open Dedge


[<System.Runtime.CompilerServices.Extension>]
type FuzzerExtensions =   
    [<System.Runtime.CompilerServices.Extension>]   
    static member GenerateVisa(fuzzer : IFuzz) =                
        let cardizer = new Cardizer(fuzzer.Random)
        cardizer.NextVisa()