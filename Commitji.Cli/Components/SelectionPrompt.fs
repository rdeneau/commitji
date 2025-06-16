module Commitji.Cli.Components.SelectionPrompt

open Commitji.Cli
open Spectre.Console

type Choice = { Code: string; Hint: string }

[<Literal>]
let private HalfPageSize = 5

[<Literal>]
let private PageSize = 2 * HalfPageSize

module private Markup =
    let promptChoice isCurrent code hint =
        if isCurrent then
            [| $" [cyan]»[/] %s{Markup.selection code}"; $"[cyan]%s{hint}[/]" |]
        else
            [| $"   [green4]%s{code}[/]"; hint |]

    let promptOtherChoice = [| "   [grey]...[/]"; "" |]

type SelectionPrompt =
    static member choice code hint = { Code = code; Hint = hint }

    static member render(currentChoiceIndex, input) =
        fun (choices: Choice array) ->
            let grid =
                Grid()
                    .AddColumn(GridColumn().NoWrap()) // Code
                    .AddColumn(GridColumn().NoWrap()) // Hint

            let addOther () =
                grid.AddRow(Markup.promptOtherChoice) |> ignore

            let props =
                let minIndex = currentChoiceIndex - HalfPageSize
                let maxIndex = currentChoiceIndex + HalfPageSize - 1

                // First half-page
                if minIndex <= 0 then
                    {|
                        hasPrevious = false
                        hasNext = PageSize < choices.Length
                        minIndex = 0
                        maxIndex = PageSize - 1
                    |}

                // Last half-page
                elif (maxIndex + 1) >= choices.Length then
                    {|
                        hasPrevious = choices.Length > PageSize
                        hasNext = false
                        minIndex = choices.Length - PageSize
                        maxIndex = choices.Length - 1
                    |}

                // Middle page, around the current choice
                else
                    {|
                        hasPrevious = minIndex > 0
                        hasNext = (maxIndex + 1) < choices.Length
                        minIndex = minIndex
                        maxIndex = maxIndex
                    |}

            for index, choice in Array.indexed choices do
                if (index = props.minIndex && props.hasPrevious) || (index = props.maxIndex && props.hasNext) then
                    addOther ()
                elif index >= props.minIndex && index <= props.maxIndex then
                    let isCurrentChoice = (index = currentChoiceIndex)
                    grid.AddRow(Markup.promptChoice isCurrentChoice (choice.Code |> Markup.highlight input) choice.Hint) |> ignore

            AnsiConsole.Write(grid)