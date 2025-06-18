module Commitji.Cli.Components.SelectionPrompt

open Commitji.Cli
open Commitji.Core
open Commitji.Core.Model.Search
open Spectre.Console

type Choice = SearchSegment<SegmentId> list

[<Literal>]
let private HalfPageSize = 5

[<Literal>]
let private PageSize = 2 * HalfPageSize

module private Markup =
    let private promptSegment isCurrent segmentId text =
        match segmentId, isCurrent with
        | SegmentId.Code, true -> Markup.selected text
        | SegmentId.Code, false -> Markup.selectable text
        | SegmentId.Number, true
        | SegmentId.Hint, true -> Markup.current text
        | SegmentId.Number, false
        | SegmentId.Hint, false -> text

    let promptChoice isCurrent choice = [|
        for index, segment in List.indexed choice do
            let selector =
                match index, isCurrent with
                | 0, true -> Markup.current " » "
                | 0, false -> "   "
                | _ -> ""

            let body =
                Markup.highlightSegment segment // ↩
                |> promptSegment isCurrent segment.Id

            selector + body
    |]

    let promptOtherChoice segmentTypes = [|
        for segmentType in segmentTypes do
            match segmentType with
            | SegmentId.Code -> "[grey]...[/]"
            | SegmentId.Number
            | SegmentId.Hint -> ""
    |]

type SelectionPrompt =
    static member codeChoice code = [ // ↩
        SearchSegment.create SegmentId.Code code (SegmentState.Searchable SearchOperation.StartsWith)
    ]

    static member render(currentChoiceIndex) =
        fun (choices: Choice array) ->
            let segmentIds = [ for segment in choices[0] -> segment.Id ]

            let grid =
                (Grid(), segmentIds)
                ||> List.fold (fun grid segmentType ->
                    let column = GridColumn().NoWrap()

                    match segmentType with
                    | SegmentId.Number -> column.Alignment <- Justify.Right
                    | _ -> ()

                    grid.AddColumn(column)
                )

            let addOther () =
                grid.AddRow(Markup.promptOtherChoice segmentIds) |> ignore

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
                    grid.AddRow(Markup.promptChoice isCurrentChoice choice) |> ignore

            AnsiConsole.Write(grid)