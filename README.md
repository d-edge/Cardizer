<br />

<p align="center">
    <img src="https://raw.githubusercontent.com/d-edge/cardizer/main/cardizer.png" alt="cardizer logo" height="140">
</p>

<p align="center">
        <a href="https://github.com/d-edge/Cardizer/actions" title="actions"><img src="https://github.com/d-edge/cardizer/actions/workflows/build.yml/badge.svg?branch=main" alt="actions build" /></a>
    <a href="https://www.nuget.org/packages/DEdge.Cardizer/" title="nuget"><img src="https://img.shields.io/nuget/vpre/DEdge.Cardizer" alt="version" /></a>
    <a href="https://www.nuget.org/stats/packages/DEdge.Cardizer?groupby=Version" title="stats"><img src="https://img.shields.io/nuget/dt/DEdge.Cardizer" alt="download" /></a> 
    <a href="https://raw.githubusercontent.com/d-edge/cardizer/main/LICENSE" title="license"><img src="https://img.shields.io/github/license/d-edge/Cardizer" alt="license" /></a>
</p>

<br />

Cardizer is a credit card randomizer to test application. Maintained by folks at [D-EDGE](https://www.d-edge.com/).

Keep in mind that all credit card numbers generated with Cardizer are completely random and do not hold any real life value.

## Features

* Easy to use
* Easy to extend
* Thread safe
* Check with the Luhn's algorithm
* Support for Visa credit card
* Support for Jcb credit card
* Support for Amex credit card
* Support for Discover credit card
* Support for MasterCard credit card

## Getting Started

Install the [DEdge.Cardizer](https://www.nuget.org/packages/DEdge.Cardizer) NuGet package:

    PM> Install-Package DEdge.Cardizer

Alternatively you can also use the .NET CLI to add the packages:

    dotnet add package DEdge.Cardizer

Next create a .net application and use DEdge.Cardizer:

```fsharp
open System

[<EntryPoint>]
let main _ =
    DEdge.Cardizer.NextVisa () |> printfn "Visa: %s"
    0
```

or in C#:

```csharp
static void Main(string[] args)
{
    var card = DEdge.Cardizer.NextVisa();
    Console.WriteLine(card);
}
```

output:

    Visa: 4127540509730813

Of course the credit card value is going to be randomized.

## Sample applications

There is a F# sample application which can be found in the [`App`](https://github.com/d-edge/Cardizer/tree/main/src/App) folder and a C# sample application which can be found in the [`AppCs`](https://github.com/d-edge/Cardizer/tree/main/src/AppCs) folder.

## Contributing

Help and feedback is always welcome and pull requests get accepted.

* First open an issue to discuss your changes
* After your change has been formally approved please submit your PR **against the main branch**
* Please follow the code convention by examining existing code
* Add/modify the `README.md` as required
* Add/modify unit tests as required
* Please document your changes in the upcoming release notes in `RELEASE_NOTES.md`
* PRs can only be approved and merged when all checks succeed (builds on Windows, MacOs and Linux)

## License

[MIT](https://raw.githubusercontent.com/d-edge/cardizer/main/LICENSE)
