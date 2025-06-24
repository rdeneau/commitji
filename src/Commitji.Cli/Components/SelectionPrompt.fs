module Commitji.Cli.Components.SelectionPrompt

open Commitji.Cli
open Commitji.Core.Model
open Commitji.Core.Model.Search
open Spectre.Console

type Choice = SearchSegment list

[<Literal>]
let private HalfPageSize = 5

module private Markup =
    let private promptSegment isCurrent segmentId text =
        match segmentId, isCurrent with
        | SegmentId.Code, true -> text |> Markup.selected |> Markup.strong
        | SegmentId.Code, false -> text |> Markup.selectable
        | SegmentId.Number, true
        | SegmentId.Hint, true -> text |> Markup.current
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

type SelectionPrompt(?halfPageSize) =
    static let defaultInstance = SelectionPrompt()

    let halfPageSize = defaultArg halfPageSize HalfPageSize
    let pageSize = 2 * halfPageSize

    member _.compute(currentChoiceIndex) =
        fun (choices: Choice list) ->
            let nbChoices = choices.Length

            let segmentIds =
                match choices with
                | [] -> []
                | firstChoice :: _ -> [ for segment in firstChoice -> segment.Id ]

            let props =
                let minIndex = currentChoiceIndex - halfPageSize
                let maxIndex = currentChoiceIndex + halfPageSize - 1

                // First half-page
                if minIndex <= 0 then
                    {|
                        hasPrevious = false
                        hasNext = pageSize < nbChoices
                        minIndex = 0
                        maxIndex = pageSize - 1
                    |}

                // Last half-page
                elif (maxIndex + 1) >= nbChoices then
                    {|
                        hasPrevious = nbChoices > pageSize
                        hasNext = false
                        minIndex = nbChoices - pageSize
                        maxIndex = nbChoices - 1
                    |}

                // Middle page, around the current choice
                else
                    {|
                        hasPrevious = minIndex > 0
                        hasNext = (maxIndex + 1) < nbChoices
                        minIndex = minIndex
                        maxIndex = maxIndex
                    |}

            {|
                rows = [
                    for index, choice in List.indexed choices do
                        if (index = props.minIndex && props.hasPrevious) || (index = props.maxIndex && props.hasNext) then
                            Markup.promptOtherChoice segmentIds
                        elif index >= props.minIndex && index <= props.maxIndex then
                            let isCurrentChoice = (index = currentChoiceIndex)
                            Markup.promptChoice isCurrentChoice choice
                ]
                segmentIds = segmentIds
            |}

    static member render(currentChoiceIndex) =
        fun (choices: Choice list) ->
            let model = defaultInstance.compute currentChoiceIndex choices

            match model.rows with
            | [] -> ()
            | _ ->
                let grid =
                    (Grid(), model.segmentIds)
                    ||> List.fold (fun grid segmentType ->
                        let column = GridColumn(NoWrap = true)

                        match segmentType with
                        | SegmentId.Number -> column.Alignment <- Justify.Right
                        | _ -> ()

                        grid.AddColumn(column)
                    )

                let grid = (grid, model.rows) ||> List.fold _.AddRow

                AnsiConsole.Write(grid)