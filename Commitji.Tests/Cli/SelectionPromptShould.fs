module Commitji.Tests.Cli.SelectionPromptShould

open Commitji.Cli.Components.SelectionPrompt
open Commitji.Core.Model
open Commitji.Core.Model.Search
open Swensen.Unquote
open global.Xunit

module ``handle 8 choices`` =
    [<AutoOpen>]
    module Fixture =
        let sut = SelectionPrompt(halfPageSize = 3)

        let private segment id text = {
            Id = id
            Text = text
            State = SegmentState.NotSearchable
        }

        let private choice num code = [ // ↩
            segment SegmentId.Number (SegmentText num)
            segment SegmentId.Code (SegmentText code)
        ]

        let choices = [ // ↩
            choice "1." "A"
            choice "2." "B"
            choice "3." "C"
            choice "4." "D"
            choice "5." "E"
            choice "6." "F"
            choice "7." "G"
            choice "8." "H"
        ]

        let verify currentChoiceIndex expected =
            let actual = sut.compute currentChoiceIndex choices
            actual.rows =! expected

    [<Fact>]
    let ``0_ support empty choices`` () =
        let actual = sut.compute 0 []
        actual.rows =! []

    module ``1_ large page (length(10) > nbChoices)`` =
        [<Fact>]
        let ``1_ indicate 1st of all`` () =
            let sut = SelectionPrompt(halfPageSize = 5)
            let actual = sut.compute 0 choices

            actual.rows
            =! [
                [| "[cyan] » [/][cyan]1.[/]"; "[bold][green1]A[/][/]" |]
                [| "   2."; "[green4]B[/]" |]
                [| "   3."; "[green4]C[/]" |]
                [| "   4."; "[green4]D[/]" |]
                [| "   5."; "[green4]E[/]" |]
                [| "   6."; "[green4]F[/]" |]
                [| "   7."; "[green4]G[/]" |]
                [| "   8."; "[green4]H[/]" |]
            ]

    module ``2_ small page (length(6) < nbChoices)`` =
        [<Fact>]
        let ``indicate 1st: »1 2 3 4 5 …`` () =
            verify 0 [
                [| "[cyan] » [/][cyan]1.[/]"; "[bold][green1]A[/][/]" |]
                [| "   2."; "[green4]B[/]" |]
                [| "   3."; "[green4]C[/]" |]
                [| "   4."; "[green4]D[/]" |]
                [| "   5."; "[green4]E[/]" |]
                [| ""; "[grey]...[/]" |]
            ]

        [<Fact>]
        let ``indicate 2nd: 1 »2 3 4 5 …`` () =
            verify 1 [
                [| "   1."; "[green4]A[/]" |]
                [| "[cyan] » [/][cyan]2.[/]"; "[bold][green1]B[/][/]" |]
                [| "   3."; "[green4]C[/]" |]
                [| "   4."; "[green4]D[/]" |]
                [| "   5."; "[green4]E[/]" |]
                [| ""; "[grey]...[/]" |]
            ]

        [<Fact>]
        let ``indicate 4th: 1 2 3 »4 5 …`` () =
            verify 3 [
                [| "   1."; "[green4]A[/]" |]
                [| "   2."; "[green4]B[/]" |]
                [| "   3."; "[green4]C[/]" |]
                [| "[cyan] » [/][cyan]4.[/]"; "[bold][green1]D[/][/]" |]
                [| "   5."; "[green4]E[/]" |]
                [| ""; "[grey]...[/]" |]
            ]

        [<Fact>]
        let ``indicate 5th: … 3 4 »5 6 …`` () =
            verify 4 [
                [| ""; "[grey]...[/]" |]
                [| "   3."; "[green4]C[/]" |]
                [| "   4."; "[green4]D[/]" |]
                [| "[cyan] » [/][cyan]5.[/]"; "[bold][green1]E[/][/]" |]
                [| "   6."; "[green4]F[/]" |]
                [| ""; "[grey]...[/]" |]
            ]

        [<Fact>]
        let ``indicate 6th: … 4 5 »6 7 8`` () =
            verify 5 [
                [| ""; "[grey]...[/]" |]
                [| "   4."; "[green4]D[/]" |]
                [| "   5."; "[green4]E[/]" |]
                [| "[cyan] » [/][cyan]6.[/]"; "[bold][green1]F[/][/]" |]
                [| "   7."; "[green4]G[/]" |]
                [| "   8."; "[green4]H[/]" |]
            ]

        [<Fact>]
        let ``indicate 8th: … 4 5 6 7 »8`` () =
            verify 7 [
                [| ""; "[grey]...[/]" |]
                [| "   4."; "[green4]D[/]" |]
                [| "   5."; "[green4]E[/]" |]
                [| "   6."; "[green4]F[/]" |]
                [| "   7."; "[green4]G[/]" |]
                [| "[cyan] » [/][cyan]8.[/]"; "[bold][green1]H[/][/]" |]
            ]