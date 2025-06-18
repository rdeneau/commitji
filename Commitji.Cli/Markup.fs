module Commitji.Cli.Markup

open Commitji.Core.Model.Search

let inline private applyMarkup markup text = // ↩
    $"[%s{markup}]%s{text}[/]"

let selected = applyMarkup "bold green1"
let selectable = applyMarkup "green4"
let current = applyMarkup "cyan"

let kbd text =
    applyMarkup "white on grey" $"[[%s{text}]]"

let highlightSegmentWith applyHighlight (segment: SearchSegment<_>) =
    let text = segment.Text

    match segment.State with
    | SegmentState.NotSearchable
    | SegmentState.Searchable _
    | SegmentState.Searched(hits = []) -> text
    | SegmentState.Searched(length = 0) -> text
    | SegmentState.Searched(hits, n) ->
        let maxHit = text.Length - n

        let hits =
            hits
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

        loop hits text

let highlightSegment segment =
    highlightSegmentWith (applyMarkup "grey19 on yellow") segment