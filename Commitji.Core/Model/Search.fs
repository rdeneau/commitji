module Commitji.Core.Model.Search

open Commitji.Core.Helpers

[<RequireQualifiedAccess>]
type SearchOperation =
    | StartsWith
    | Contains

/// <remarks>
/// This union could be split into two for a more precise modelling,
/// to know when the search happened, but it's too much complexity in the end.
/// </remarks>
[<RequireQualifiedAccess>]
type SegmentState =
    | NotSearchable
    | Searchable of operation: SearchOperation
    | Searched of hits: int list * length: int

type SearchSegment<'id> = {
    Id: 'id
    Text: string
    State: SegmentState
}

[<RequireQualifiedAccess>]
module SearchSegment =
    let create id text state = {
        Id = id
        Text = text
        State = state
    }

type SearchItem<'t, 'id> = {
    Item: 't
    Index: int
    Segments: SearchSegment<'id> list
}

type SearchableList<'t, 'id> = SearchItem<'t, 'id> list

[<RequireQualifiedAccess>]
type SearchInput =
    private
    | NotEmpty of input: string

    member this.Value =
        match this with
        | NotEmpty input -> input

    member this.Length = this.Value.Length

    /// <summary>
    /// Create a new <see cref="SearchInput"/> from the given <paramref name="input"/>.
    /// </summary>
    /// <param name="input">The search input value</param>
    /// <returns>An <c>Option</c> containing the created <see cref="SearchInput"/>
    ///          or <c>None</c> if the given <paramref name="input"/> is empty.</returns>
    static member tryCreate input =
        match input with
        | String.IsEmpty -> None
        | String.IsNotEmpty -> Some(SearchInput.NotEmpty input)

    /// <summary>
    /// Create a new <see cref="SearchInput"/> from the given <paramref name="notEmptyInput"/>.
    /// </summary>
    /// <param name="notEmptyInput">The search input value, not empty otherwise 💥</param>
    /// <exception cref="ArgumentException">When the given <paramref name="notEmptyInput"/> is null or empty</exception>
    static member create notEmptyInput =
        match SearchInput.tryCreate notEmptyInput with
        | Some searchInput -> searchInput
        | None -> invalidArg (nameof notEmptyInput) "Cannot be empty"

type Search<'t, 'id>(initSegmentsByIndex: int -> 't -> SearchSegment<'id> list) =
    let buildResult (searchItem: int -> 't -> SearchItem<'t, 'id> option) (items: 't list) =
        // We call `searchItem` twice:
        // 1. First to filter out items that do not match the search.
        // 2. Second to the items with the right index.
        items
        |> Seq.filter (fun item -> searchItem 0 item |> Option.isSome)
        |> Seq.indexed
        |> Seq.choose (fun (index, item) -> searchItem index item)
        |> Seq.toList

    let searchItem input comparison index item =
        let length = String.length input

        let segments = [
            for segment in initSegmentsByIndex index item do
                match segment.State with
                | SegmentState.Searchable location -> // Searchable, update state with search results
                    let hits =
                        match location with
                        | SearchOperation.StartsWith when segment.Text.StartsWith(input, comparison) -> [ 0 ] // Match at the start
                        | SearchOperation.StartsWith -> [] // No match
                        | SearchOperation.Contains -> segment.Text |> String.allIndexesOf input comparison

                    { segment with State = SegmentState.Searched(hits, length) }

                | SegmentState.NotSearchable -> // ↩
                    { segment with State = SegmentState.NotSearchable }

                | SegmentState.Searched _ -> failwith "invalid state before search"
        ]

        let hasMatchingSegments =
            segments
            |> List.exists (fun segment ->
                match segment.State with
                | SegmentState.NotSearchable
                | SegmentState.Searched(hits = [])
                | SegmentState.Searched(length = 0) -> false
                | SegmentState.Searched _ -> true
                | SegmentState.Searchable _ -> failwith "invalid state after search"
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

    member this.Run(input: SearchInput, items: 't list, comparison) : SearchableList<'t, 'id> =
        items // ↩
        |> buildResult (searchItem input.Value comparison)