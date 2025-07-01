module Commitji.Core.Model.Search

open System
open Commitji.Core.Helpers

[<RequireQualifiedAccess>]
type SearchOperation =
    | StartsWith
    | Contains

    member this.AllIndexesOf(input: string, text: string, comparison: StringComparison) =
        match this with
        | StartsWith when text.StartsWith(input, comparison) -> [ 0 ] // Match at the start
        | StartsWith -> [] // No match
        | Contains -> text |> String.allIndexesOf input comparison

type SegmentText =
    | SegmentText of text: string
    | SegmentTexts of texts: string list

    member this.Value =
        match this with
        | SegmentText text -> text
        | SegmentTexts(text :: _) -> text
        | SegmentTexts _ -> ""

    member this.Values =
        match this with
        | SegmentText text -> [ text ]
        | SegmentTexts texts -> texts

    static member create texts =
        match texts with
        | [ text ] -> SegmentText text
        | texts -> SegmentTexts texts

type SearchedSegmentChunk = {
    Text: string
    Hits: int list
} with
    member this.HasNoHits = this.Hits.IsEmpty

type SearchedSegmentText =
    | SearchedSegmentText of SearchedSegmentChunk
    | SearchedSegmentTexts of SearchedSegmentChunk list

    member private this.Normalize(normalizeChunks) =
        match this with
        | SearchedSegmentText chunk
        | SearchedSegmentTexts [ chunk ] -> SearchedSegmentText chunk
        | SearchedSegmentTexts chunks -> SearchedSegmentTexts(normalizeChunks chunks)

    member this.Normalize(sortChunks) =
        this.Normalize(if sortChunks then List.sortByDescending _.Hits.Length else id) // Get chunks with max hits first

[<RequireQualifiedAccess>]
module SearchedSegmentText =
    let (|HasNoHits|_|) (searchedSegmentText: SearchedSegmentText) =
        match searchedSegmentText with
        | SearchedSegmentText x -> x.HasNoHits
        | SearchedSegmentTexts xs -> xs |> List.forall _.HasNoHits

[<AutoOpen>]
module private SegmentTextExtensions =
    type SegmentText with
        member this.Search(operation: SearchOperation, input, comparison) =
            match this with
            | SegmentText text -> SearchedSegmentText { Text = text; Hits = operation.AllIndexesOf(input, text, comparison) }
            | SegmentTexts texts -> (SearchedSegmentTexts [ for text in texts -> { Text = text; Hits = operation.AllIndexesOf(input, text, comparison) } ]).Normalize(sortChunks = true)

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

/// <summary>
/// State-based variations of the combinations of <c>SegmentId</c> and <c>SegmentText</c>, with additional fields according to the state of the segment.
/// </summary>
type SearchSegment =
    | NotSearchable (**) of id: SegmentId * text: SegmentText
    | Searchable (*****) of id: SegmentId * text: SegmentText * operation: SearchOperation
    | Searched (*******) of id: SegmentId * text: SearchedSegmentText * length: int // length of the input used for the search
    | Selected (*******) of id: SegmentId * text: SegmentText

    member this.Id =
        match this with
        | NotSearchable(id, _)
        | Searchable(id, _, _)
        | Searched(id, _, _)
        | Selected(id, _) -> id

    member this.Text =
        match this with
        | NotSearchable(_, text)
        | Searchable(_, text, _)
        | Selected(_, text) -> text
        | Searched(_, SearchedSegmentText x, _)
        | Searched(_, SearchedSegmentTexts [ x ], _) -> SegmentText x.Text
        | Searched(_, SearchedSegmentTexts xs, _) -> SegmentTexts([ for x in xs |> List.sortBy _.HasNoHits -> x.Text ]) // 👈 To place the eventual chunk found at the top of the list

    member this.AsSelected =
        match this with
        | NotSearchable(id, text) -> Selected(id, text)
        | Searchable(id, text, _) -> Selected(id, text)
        | Searched(id, SearchedSegmentText chunk, _)
        | Searched(id, SearchedSegmentTexts [ chunk ], _) -> Selected(id, SegmentText chunk.Text)
        | Searched(id, SearchedSegmentTexts chunks, _) -> Selected(id, SegmentTexts([ for chunk in chunks -> chunk.Text ]))
        | Selected _ -> this // Already selected

    static member SearchableByStart(id, text) =
        SearchSegment.Searchable(id, text, SearchOperation.StartsWith)

    static member SearchableByContent(id, text) =
        SearchSegment.Searchable(id, text, SearchOperation.Contains)

type SearchItem<'t> = {
    Item: 't
    Index: int
    Segments: SearchSegment list
} with
    member this.AsSelected = { this with Segments = [ for segment in this.Segments -> segment.AsSelected ] }

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
                match segment with
                | SearchSegment.Searchable(id, text, operation) -> // ↩
                    SearchSegment.Searched(id, text.Search(operation, input, comparison), length)

                | SearchSegment.NotSearchable _ -> segment

                | SearchSegment.Searched _ -> raiseInvalidSegmentState (nameof SearchSegment.Searched) "before search" segment.Id item
                | SearchSegment.Selected _ -> raiseInvalidSegmentState (nameof SearchSegment.Selected) "before search" segment.Id item
        ]

        let hasMatchingSegments =
            segments
            |> List.exists (fun segment ->
                match segment with
                | SearchSegment.NotSearchable _
                | SearchSegment.Searched(text = SearchedSegmentText.HasNoHits)
                | SearchSegment.Searched(length = 0) -> false
                | SearchSegment.Searched _ -> true
                | SearchSegment.Searchable _ -> raiseInvalidSegmentState (nameof SearchSegment.Searchable) "after search" segment.Id item
                | SearchSegment.Selected _ -> raiseInvalidSegmentState (nameof SearchSegment.Selected) "before selection" segment.Id item
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