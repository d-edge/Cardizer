namespace DEdge

open System
open System.Threading
open System.Runtime.InteropServices

[<Interface>]
type IRandom =
    abstract Next: maxValue: int -> int

type ThreadLocalRandom(seedGenerator: Random) = 

    // original snippet by @tpetricek
    // https://stackoverflow.com/a/7792667/1248177
    let localGenerator =
        new ThreadLocal<Random>(fun _ ->
            lock
                seedGenerator
                (fun _ ->
                    let seed = seedGenerator.Next()
                    Random(seed)))
    
    new() =
        ThreadLocalRandom(Random())

    new(seed: int) =
        ThreadLocalRandom(Random(seed))

    interface IRandom with 
        member this.Next(maxValue) = 
            localGenerator.Value.Next(maxValue)

