module Commitji.Core.Model.Search

open Commitji.Core.Helpers

type SearchSegment<'id, 'state> = {
    Id: 'id
    Text: string

    /// Either `SegmentInitState` or `SegmentStateAfterSearch`
    State: 'state
}

[<RequireQualifiedAccess>]
module SearchSegment =
    let create id text state = {
        Id = id
        Text = text
        State = state
    }

    let changeState (newState: 'newState) (segment: SearchSegment<'id, 'state>) = {
        Id = segment.Id
        Text = segment.Text
        State = newState
    }

[<RequireQualifiedAccess>]
type SearchOperation =
    | StartsWith
    | Contains

[<RequireQualifiedAccess>]
type SegmentInitState =
    | NotSearchable
    | Searchable of operation: SearchOperation

[<RequireQualifiedAccess>]
type SegmentStateAfterSearch =
    | NotSearchable
    | Searched of hits: int list

type SearchItem<'t, 'id, 'state> = {
    Item: 't
    Index: int
    Segments: SearchSegment<'id, 'state> list
}

type SearchableList<'t, 'id> = SearchItem<'t, 'id, SegmentInitState> list
type SearchedList<'t, 'id> = SearchItem<'t, 'id, SegmentStateAfterSearch> list

[<RequireQualifiedAccess>]
type SearchInput =
    private
    | NotEmpty of input: string

    member this.Value =
        match this with
        | NotEmpty input -> input

    static member tryCreate input =
        match input with
        | String.IsEmpty -> None
        | String.IsNotEmpty -> Some(SearchInput.NotEmpty input)

    static member create input =
        match SearchInput.tryCreate input with
        | Some searchInput -> searchInput
        | None -> invalidArg (nameof input) "Cannot be empty"

type Search<'t, 'id>(initSegmentsByIndex: int -> 't -> SearchSegment<'id, SegmentInitState> list) =
    let buildResult (searchItem: int -> 't -> SearchItem<'t, 'id, 'state> option) (items: 't list) =
        // We call `searchItem` twice:
        // 1. First to filter out items that do not match the search.
        // 2. Second to the items with the right index.
        items
        |> Seq.filter (fun item -> searchItem 0 item |> Option.isSome)
        |> Seq.indexed
        |> Seq.choose (fun (index, item) -> searchItem index item)
        |> Seq.toList

    let searchItem input comparison index item =
        let segments = [
            for segment in initSegmentsByIndex index item do
                match segment.State with
                | SegmentInitState.Searchable location -> // Searchable, update state with search results
                    let hits =
                        match location with
                        | SearchOperation.StartsWith when segment.Text.StartsWith(input, comparison) -> [ 0 ] // Match at the start
                        | SearchOperation.StartsWith -> [] // No match
                        | SearchOperation.Contains -> segment.Text |> String.allIndexesOf input comparison

                    segment |> SearchSegment.changeState (SegmentStateAfterSearch.Searched hits)

                | SegmentInitState.NotSearchable -> // ↩
                    segment |> SearchSegment.changeState SegmentStateAfterSearch.NotSearchable
        ]

        let hasMatchingSegments =
            segments
            |> List.exists (fun segment ->
                match segment.State with
                | SegmentStateAfterSearch.NotSearchable
                | SegmentStateAfterSearch.Searched [] -> false
                | SegmentStateAfterSearch.Searched _ -> true
            )

        match hasMatchingSegments with
        | false -> None
        | true ->
            Some {
                Item = item
                Index = index
                Segments = segments
            }

    member _.Init(items) : SearchableList<'t, 'id> =
        items
        |> buildResult (fun index item ->
            Some {
                Item = item
                Index = index
                Segments = initSegmentsByIndex index item
            }
        )

    member this.Run(input: SearchInput, items: 't list, comparison) : SearchedList<'t, 'id> =
        items // ↩
        |> buildResult (searchItem input.Value comparison)