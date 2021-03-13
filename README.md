![Cardizer](https://raw.githubusercontent.com/d-edge/cardizer/main/cardizer.png)

# Cardizer

A functional minimalist credit card randomizer to test application.

## Features

* Easy to use
* Easy to extend
* Thread safe
* Check with the Luhn's algorithm
* Support for Visa credit card
* Support for MasterCard credit card

## Getting Started

Install the [Dedge.Cardizer](https://www.nuget.org/packages/Dedge.Cardizer) NuGet package:

    PM> Install-Package Dedge.Cardizer

Alternatively you can also use the .NET CLI to add the packages:

    dotnet add package Dedge.Cardizer

Next create a .net application and use Dedge.Cardizer:

```fsharp
open System

[<EntryPoint>]
let main _ =
    Dedge.Cardizer.generateVisa () |> printfn "Visa: %s"
    0
```

output:

    Visa: 4127540509730813

Of course the credit card value is going to be randomized.

## Sample applications

There is a sample application which can be found in the [`App`](https://github.com/d-edge/Cardizer/tree/main/src/App) folder.

## Contributing

Help and feedback is always welcome and pull requests get accepted.

- First open an issue to discuss your changes
- After your change has been formally approved please submit your PR **against the develop branch**
- Please follow the code convention by examining existing code
- Add/modify the `README.md` as required
- Add/modify unit tests as required
- Please document your changes in the upcoming release notes in `RELEASE_NOTES.md`
- PRs can only be approved and merged when all checks succeed (builds on Windows, MacOs and Linux)

## License

[MIT](https://raw.githubusercontent.com/d-edge/cardizer/main/LICENSE)
