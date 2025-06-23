module Commitji.Core.Model.Search

open System
open Commitji.Core.Helpers

[<RequireQualifiedAccess>]
type SearchOperation =
    | StartsWith
    | Contains

type SegmentText =
    | SegmentText of string

    member this.Value =
        match this with
        | SegmentText text -> text

/// <remarks>
/// This union could be split into two for a more precise modelling,
/// to know when the search happened, but it's too much complexity in the end.
/// </remarks>
[<RequireQualifiedAccess>]
type SegmentState =
    | NotSearchable
    | Searchable of operation: SearchOperation
    | Searched of hits: int list * length: int
    | Selected

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

type SearchSegment = {
    Id: SegmentId
    Text: SegmentText
    State: SegmentState
}

[<RequireQualifiedAccess>]
module SearchSegment =
    let create id text state = {
        Id = id
        Text = text
        State = state
    }

type SearchSegment with
    static member NotSearchable(id, text) =
        SearchSegment.create id text SegmentState.NotSearchable

    static member SearchableByStart(id, text) =
        SearchSegment.create id text (SegmentState.Searchable SearchOperation.StartsWith)

    static member SearchableByContent(id, text) =
        SearchSegment.create id text (SegmentState.Searchable SearchOperation.Contains)

    static member Searched(id, text, searchInput: SearchInput, hits) =
        SearchSegment.create id text (SegmentState.Searched(hits, searchInput.Length))

    static member NotFound(id, text, searchInput: SearchInput) =
        SearchSegment.Searched(id, text, searchInput, hits = [])

    static member Found(id, text, searchInput: SearchInput, firstHit, [<ParamArray>] otherHits) =
        SearchSegment.Searched(id, text, searchInput, hits = firstHit :: List.ofArray otherHits)

type SearchItem<'t> = {
    Item: 't
    Index: int
    Segments: SearchSegment list
} with
    member this.AsSelected = {
        this with
            Segments = [
                for segment in this.Segments do
                    {
                        segment with
                            Text =
                                match segment.Id with
                                | SegmentId.Number -> SegmentText ""
                                | _ -> segment.Text
                            State = SegmentState.Selected
                    }
            ]
    }

[<RequireQualifiedAccess>]
module SearchItem =
    let create item index segments = {
        Item = item
        Index = index
        Segments = segments
    }

    let init item = create item 0 []

type SearchableList<'t> = SearchItem<'t> list

[<RequireQualifiedAccess>]
module SearchableList =
    let init segments items = [
        for index, item in Seq.indexed items do
            SearchItem.create item index (segments index item)
    ]

type Search<'t>(initSegmentsByIndex: int -> 't -> SearchSegment list) =
    let buildResult (searchItem: int -> 't -> SearchItem<'t> option) (items: 't list) =
        items
        |> Seq.indexed
        |> Seq.choose (fun (index, item) -> searchItem index item)
        |> Seq.indexed
        |> Seq.map (fun (index, item) -> { item with Index = index })
        |> Seq.toList

    let raiseInvalidSegmentState stateName context segmentId item =
        failwithf $"invalid state '%s{stateName}' %s{context} for segment %A{segmentId} for item %A{item}"

    let searchItem input comparison index item =
        let length = String.length input

        let segments = [
            for segment in initSegmentsByIndex index item do
                match segment.State with
                | SegmentState.Searchable location ->
                    let hits =
                        match location with
                        | SearchOperation.StartsWith when segment.Text.Value.StartsWith(input, comparison) -> [ 0 ] // Match at the start
                        | SearchOperation.StartsWith -> [] // No match
                        | SearchOperation.Contains -> segment.Text.Value |> String.allIndexesOf input comparison

                    { segment with State = SegmentState.Searched(hits, length) }

                | SegmentState.NotSearchable -> // ↩
                    { segment with State = SegmentState.NotSearchable }

                | SegmentState.Searched _ -> raiseInvalidSegmentState (nameof SegmentState.Searched) "before search" segment.Id item
                | SegmentState.Selected -> raiseInvalidSegmentState (nameof SegmentState.Selected) "before search" segment.Id item
        ]

        let hasMatchingSegments =
            segments
            |> List.exists (fun segment ->
                match segment.State with
                | SegmentState.NotSearchable
                | SegmentState.Searched(hits = [])
                | SegmentState.Searched(length = 0) -> false
                | SegmentState.Searched _ -> true
                | SegmentState.Searchable _ -> raiseInvalidSegmentState (nameof SegmentState.Searchable) "after search" segment.Id item
                | SegmentState.Selected -> raiseInvalidSegmentState (nameof SegmentState.Selected) "before selection" segment.Id item
            )

        match hasMatchingSegments with
        | false -> None
        | true -> Some(SearchItem.create item index segments)

    member _.Init(items) : SearchableList<'t> =
        items // ↩
        |> buildResult (fun index item -> Some(SearchItem.create item index (initSegmentsByIndex index item)))

    member this.Run(input: SearchInput, items: 't list, comparison) : SearchableList<'t> =
        items // ↩
        |> buildResult (searchItem input.Value comparison)