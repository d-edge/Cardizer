# Contributing to Cardizer

:+1::tada: First off, thanks for taking the time to contribute! :tada::+1:

The following is a set of guidelines for contributing to Cardizer, which are hosted in the [D-EDGE Organization](https://github.com/d-edge) on GitHub. These are mostly guidelines, not rules. Use your best judgment, and feel free to propose changes to this document in a pull request.

## Code of Conduct

This project and everyone participating in it is governed by the [D-EDGE Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to [softwarecraft@d-edge.com](mailto:softwarecraft@d-edge.com).

## Styleguides

### Git Commit Messages

Cardizer follows the [Conventional commits specification](https://www.conventionalcommits.org/en/v1.0.0/). You can rely on [commitizen](https://commitizen-tools.github.io/commitizen/) to help format your commit message.

### F# Styleguide

All F# code should follow the official [F# Style Guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/).

## Add a new card

In a library like Cardizer, one of the main way to contribute is by adding a new credit card. Below is a step-by-step guide:

- Select a card with an empty cell under the Progression column from the [issue #2](https://github.com/d-edge/Cardizer/issues/2). Lets say Xxx.
- Clone the project and checkout the `develop` branch
- From `develop` create a new branch named `feature/addXxx`
- Write a card generator for Xxx
  - Open `src/DEdge.Cardizer/DEdge.Cardizer.fs`
  - Add a function for your card:

```fsharp
let NextXxx () =
    let prefixes = [ 4 ] // 4 is the visa's prefix. Use Xxx's prefixes.
    let checksum = 8 // 8 is the checksum of the prefixes
    let length = 14 // 14 is the number of digits to generate 
    generateCard prefixes sum length
```

_If prefixes is a range, you can use the helper `Cardizer.NextInRange` to get the next random integer in an inclusive range._

- Test your code
  - Open `src/Tests/Test.fs`
  - Add a unit test for your generator:

```fsharp
[<Fact>]
let ``Should generate valid Xxx`` () =
    let card = Cardizer.NextXxx ()
    Assert.StartsWith("4", card) // replace 4 by Xxx's prefixes
    Assert.Equal(16, card.Length) // replace 16 by Xxx's length
    Assert.True(luhn card, $"The credit card number {card} failed the Luhn Check.")
```

- Test your test with `dotnet test`
- Push you branch to [d-edge/Cardizer](https://github.com/d-edge/Cardizer) and open a Pull Request.

Thank you!
