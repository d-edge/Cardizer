module Tests

open System
open Diverse
open Dedge.Cardizer.Diverse
open Xunit
open FsUnit.Xunit

module Fixtures =
    type FuzzerExtensionsTestFixture() =
        do
            Fuzzer.Log <- fun log -> Console.WriteLine(log) |> ignore
        interface IDisposable with
            member _.Dispose() = 
                ()

module Tests =
    type FuzzerExtensionTests() =
        [<Fact>]
        member _.``Should generate a Visa from Fuzzer`` () =
            let fuzzer = new Fuzzer(12)
            let card = fuzzer.GenerateVisa()
            card |> should equal "4547729151676"

        interface IClassFixture<Fixtures.FuzzerExtensionsTestFixture>
