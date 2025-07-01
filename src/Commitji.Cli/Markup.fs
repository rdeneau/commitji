module Commitji.Cli.Markup

open Commitji.Core.Model.Search

let inline applyMarkup markup text = // ↩
    $"[%s{markup}]%s{text}[/]"

let current = applyMarkup "cyan"
let inactive = applyMarkup "grey"
let light = applyMarkup "silver"
let selected = applyMarkup "green1"
let selectedDim = applyMarkup "green3_1"
let selectable = applyMarkup "green4"

let error = applyMarkup "red"
let em = applyMarkup "italic"
let strong = applyMarkup "bold"

let kbd key =
    applyMarkup "white on grey" $"[[%s{key}]]"

let keyStroke keyStroke =
    [ for k in keyStroke -> kbd k ] |> String.concat "+"

let highlightSegmentWith applyHighlight (segment: SearchSegment) =
    let text = segment.Text.Value

    match segment with
    | SearchSegment.NotSearchable _
    | SearchSegment.Searchable _
    | SearchSegment.Selected _ -> text
    | SearchSegment.Searched(text = SearchedSegmentText.HasNoHits) -> text
    | SearchSegment.Searched(length = 0) -> text
    | SearchSegment.Searched(_, text, n) ->
        let chunk =
            match text with
            | SearchedSegmentText chunk -> chunk
            | SearchedSegmentTexts chunks -> chunks |> List.head // Take the first chunk, as chunks are sorted by number of hits descending

        let maxHit = chunk.Text.Length - n

        let hits =
            chunk.Hits
            |> List.filter (fun hit -> hit >= 0 && hit <= maxHit) // Filter out invalid hits
            |> List.sortDescending // Sort hits by descending order to process from right to left and avoid overlapping issues

        let rec loop hits (text: string) =
            match hits with
            | [] -> text
            | i :: rest ->
                let before = text[.. i - 1]
                let found = text[i .. i + n - 1]
                let after = text[i + n ..]

                $"%s{loop rest before}%s{applyHighlight found}%s{after}"

        loop hits chunk.Text

let highlightSegment segment =
    highlightSegmentWith (applyMarkup "grey19 on yellow") segment