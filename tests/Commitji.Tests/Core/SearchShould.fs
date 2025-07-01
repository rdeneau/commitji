module Commitji.Tests.Core.SearchShould

open System
open Commitji.Core
open Commitji.Core.Helpers
open Commitji.Core.Model
open Commitji.Core.Model.Search
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open global.Xunit

[<AutoOpen>]
module Item =
    [<RequireQualifiedAccess>]
    type ItemType =
        | Prefix of Prefix
        | Emoji of Emoji
        | BreakingChange of bool

    type Item = {
        Type: ItemType
        Codes: string list
        Label: string
        Index: int
    } with
        member item.Num = $"%i{item.Index + 1}"

        member item.ToSearchSegment(segmentId, state) =
            let segmentText =
                match segmentId with
                | SegmentId.Number -> SegmentText item.Num
                | SegmentId.Code -> SegmentText.create item.Codes
                | SegmentId.Hint -> SegmentText item.Label

            match state with
            | SegmentConfig.NotSearchable -> SearchSegment.NotSearchable(segmentId, segmentText)
            | SegmentConfig.Searchable operation -> SearchSegment.Searchable(segmentId, segmentText, operation)

    let (|PrefixItem|) (prefix: Prefix) = {
        Type = ItemType.Prefix prefix
        Codes = [ prefix.Code ]
        Label = prefix.Hint
        Index = 0
    }

    let (|EmojiItem|) (emoji: Emoji) = {
        Type = ItemType.Emoji emoji
        Codes = emoji.Codes
        Label = emoji.Hint
        Index = 0
    }

    let (|BreakingChangeItem|) (breakingChange: bool) = {
        Type = ItemType.BreakingChange breakingChange
        Codes = [
            if breakingChange then "Yes" else "No"
        ]
        Label = ""
        Index = 0
    }

    let (|AnyItem|) itemType =
        match itemType with
        | ItemType.Prefix(PrefixItem item) -> item
        | ItemType.Emoji(EmojiItem item) -> item
        | ItemType.BreakingChange(BreakingChangeItem item) -> item

    let (|ManyItems|) (NonEmptySet itemTypes) = [
        for index, AnyItem item in Seq.indexed itemTypes do
            { item with Index = index }
    ]

[<AutoOpen>]
module ``1_ init - helpers`` =
    [<RequireQualifiedAccess>]
    type Segments =
        | CodeOnly
        | All

    [<RequireQualifiedAccess>]
    type Search =
        | ByNum
        | ByCodeStart
        | ByCodeContent
        | ByLabelContent
        | FullText

    type SearchSegments = {
        Item: Item
        NumState: SegmentConfig option
        CodeState: SegmentConfig option
        LabelState: SegmentConfig option
    } with
        static member Init(item) = {
            Item = item
            NumState = None
            CodeState = None
            LabelState = None
        }

        member this.WithNumState state = { this with NumState = Some state }
        member this.WithCodeState state = { this with CodeState = Some state }
        member this.WithLabelState state = { this with LabelState = Some state }

        member this.All(state) = {
            Item = this.Item
            NumState = Some state
            CodeState = Some state
            LabelState = Some state
        }

        member this.AllNotSearchable() = this.All(SegmentConfig.NotSearchable)

        member this.ToList() =
            let searchSegments state segmentId =
                match state with
                | Some state -> [ this.Item.ToSearchSegment(segmentId, state) ]
                | None -> []

            [
                yield! searchSegments this.NumState SegmentId.Number // ↩
                yield! searchSegments this.CodeState SegmentId.Code
                yield! searchSegments this.LabelState SegmentId.Hint
            ]

    let initSegmentsByIndex segments searchType =
        let finalizeBuild (searchSegments: SearchSegments) =
            match segments, searchType with
            | Segments.CodeOnly, (Search.ByNum | Search.ByLabelContent) -> // ↩
                searchSegments.WithCodeState(SegmentConfig.NotSearchable).ToList()

            | Segments.CodeOnly, Search.ByCodeStart -> // ↩
                searchSegments.WithCodeState(SegmentConfig.Searchable SearchOperation.StartsWith).ToList()

            | Segments.CodeOnly, Search.ByCodeContent -> // ↩
                searchSegments.WithCodeState(SegmentConfig.Searchable SearchOperation.Contains).ToList()

            | Segments.CodeOnly, Search.FullText -> // ↩
                searchSegments.WithCodeState(SegmentConfig.Searchable SearchOperation.Contains).ToList()

            | Segments.All, Search.ByNum -> // ↩
                searchSegments.AllNotSearchable().WithNumState(SegmentConfig.Searchable SearchOperation.StartsWith).ToList()

            | Segments.All, Search.ByCodeStart -> // ↩
                searchSegments.AllNotSearchable().WithCodeState(SegmentConfig.Searchable SearchOperation.StartsWith).ToList()

            | Segments.All, Search.ByCodeContent -> // ↩
                searchSegments.AllNotSearchable().WithCodeState(SegmentConfig.Searchable SearchOperation.Contains).ToList()

            | Segments.All, Search.ByLabelContent -> // ↩
                searchSegments.AllNotSearchable().WithLabelState(SegmentConfig.Searchable SearchOperation.Contains).ToList()

            | Segments.All, Search.FullText ->
                searchSegments // ↩
                    .WithNumState(SegmentConfig.Searchable SearchOperation.StartsWith)
                    .WithCodeState(SegmentConfig.Searchable SearchOperation.Contains)
                    .WithLabelState(SegmentConfig.Searchable SearchOperation.Contains)
                    .ToList()

        fun _index (item: Item) ->
            let searchSegments = SearchSegments.Init(item)
            finalizeBuild searchSegments

module ``1_ init`` =
    [<Property>]
    let ``code only segment`` (ManyItems items) searchType = // ↩
        let createSearchSegment =
            match searchType with
            | Search.ByNum
            | Search.ByLabelContent -> SearchSegment.NotSearchable
            | Search.ByCodeStart -> fun (id, text) -> SearchSegment.Searchable(id, text, SearchOperation.StartsWith)
            | Search.ByCodeContent
            | Search.FullText -> fun (id, text) -> SearchSegment.Searchable(id, text, SearchOperation.Contains)

        let expected = [
            for index, item in List.indexed items do
                {
                    Item = item
                    Index = index
                    Segments = [ createSearchSegment (SegmentId.Code, SegmentText.create item.Codes) ]
                }
        ]

        let actual = Search(initSegmentsByIndex Segments.CodeOnly searchType).Init(items)

        actual =! expected

    [<Property>]
    let ``all segments`` (ManyItems items) searchType = // ↩
        let expected = [
            for index, item in List.indexed items do
                {
                    Item = item
                    Index = index
                    Segments = [
                        match searchType with
                        | Search.ByNum ->
                            SearchSegment.Searchable(SegmentId.Number, SegmentText item.Num, SearchOperation.StartsWith)
                            SearchSegment.NotSearchable(SegmentId.Code, SegmentText.create item.Codes)
                            SearchSegment.NotSearchable(SegmentId.Hint, SegmentText item.Label)

                        | Search.ByCodeStart ->
                            SearchSegment.NotSearchable(SegmentId.Number, SegmentText item.Num)
                            SearchSegment.Searchable(SegmentId.Code, SegmentText.create item.Codes, SearchOperation.StartsWith)
                            SearchSegment.NotSearchable(SegmentId.Hint, SegmentText item.Label)

                        | Search.ByCodeContent ->
                            SearchSegment.NotSearchable(SegmentId.Number, SegmentText item.Num)
                            SearchSegment.Searchable(SegmentId.Code, SegmentText.create item.Codes, SearchOperation.Contains)
                            SearchSegment.NotSearchable(SegmentId.Hint, SegmentText item.Label)

                        | Search.ByLabelContent ->
                            SearchSegment.NotSearchable(SegmentId.Number, SegmentText item.Num)
                            SearchSegment.NotSearchable(SegmentId.Code, SegmentText.create item.Codes)
                            SearchSegment.Searchable(SegmentId.Hint, SegmentText item.Label, SearchOperation.Contains)

                        | Search.FullText ->
                            SearchSegment.Searchable(SegmentId.Number, SegmentText item.Num, SearchOperation.StartsWith)
                            SearchSegment.Searchable(SegmentId.Code, SegmentText.create item.Codes, SearchOperation.Contains)
                            SearchSegment.Searchable(SegmentId.Hint, SegmentText item.Label, SearchOperation.Contains)
                    ]
                }
        ]

        let actual = Search(initSegmentsByIndex Segments.All searchType).Init(items)

        actual =! expected

[<AutoOpen>]
module ``2_ run - common - helpers`` =
    [<RequireQualifiedAccess>]
    module SearchableList =
        let autoIndex (list: SearchableList<'t>) =
            list |> List.mapi (fun index item -> { item with Index = index })

    [<RequireQualifiedAccess>]
    type Case =
        | Lower
        | Upper
        | Camel

    let applyTo input inputCase =
        let changeCase (s: string) =
            match inputCase with
            | Case.Lower -> s.ToLowerInvariant()
            | Case.Upper -> s.ToUpperInvariant()
            | Case.Camel -> s

        let removeLeadingUnderscore (s: string) =
            if s.StartsWith "_" then s.Substring(1) else s

        $"%A{input}" |> removeLeadingUnderscore |> changeCase

    type Expectation =
        | RemainNotSearchable
        | BeSearched of hits: int list

        static member NotMatch = Expectation.BeSearched(hits = [])

    type ExpectedSearchResult = ExpectedSearchResult of text: string * Expectation

    type String with
        member text.Should expectation = ExpectedSearchResult(text, expectation)

        member text.ShouldMatchAt(firstHit: int, [<ParamArray>] otherHits: int array) =
            text.Should(BeSearched(hits = [ firstHit; yield! otherHits ]))

        member text.ShouldMatchAtTheStart = text.ShouldMatchAt(0)
        member text.ShouldNotMatch = text.Should(BeSearched(hits = []))

    type Item with
        /// <remarks>
        /// ⚠️ The `Index` is set to -1 → Call `SearchableList.autoIndex` on the list to set the correct index.<br/>
        /// 💡 For `defaultValue`, use `Expectation.RemainNotSearchable` (normal search) or `Expectation.NotMatch` (full-text search).<br/>
        /// </remarks>
        member item.ToSearchItem
            (
                len: int, // ↩
                numResult: ExpectedSearchResult option,
                codeResult: ExpectedSearchResult option,
                codesResults: ExpectedSearchResult list option,
                labelResult: ExpectedSearchResult option,
                defaultExpectation: Expectation
            ) =
            let searchSegment result segmentId text =
                match defaultArg result (ExpectedSearchResult(text, defaultExpectation)) with
                | ExpectedSearchResult(text, Expectation.RemainNotSearchable) -> SearchSegment.NotSearchable(segmentId, SegmentText text)
                | ExpectedSearchResult(text, Expectation.BeSearched hits) -> SearchSegment.Searched(segmentId, SearchedSegmentText { Text = text; Hits = hits }, len)

            let codesResults =
                match codesResults, codeResult, defaultExpectation with
                | Some [], _, _ -> invalidArg (nameof codesResults) $"should not be empty, given item %A{item}"
                | Some codesResults, _, _ -> codesResults
                | None, Some codeResult, _ ->
                    match item.Codes with
                    | [] -> invalidArg (nameof item) $"should have codes: %A{item}"
                    | _ :: otherCodes -> [
                        codeResult
                        for code in otherCodes do
                            code.ShouldNotMatch
                      ]
                | None, None, Expectation.RemainNotSearchable
                | None, None, Expectation.BeSearched [] -> [ for code in item.Codes -> ExpectedSearchResult(code, defaultExpectation) ]
                | None, None, Expectation.BeSearched _ -> failwith $"defaultExpectation %A{defaultExpectation} argument can be only `Expectation.RemainNotSearchable` or `Expectation.NotMatch`"

            let codeSearchSegment =
                match codesResults.Head with
                | ExpectedSearchResult(_, Expectation.RemainNotSearchable) -> SearchSegment.NotSearchable(SegmentId.Code, SegmentText.create [ for ExpectedSearchResult(code, _) in codesResults -> code ])
                | _ ->
                    let segmentChunks = [
                        for ExpectedSearchResult(code, result) in codesResults do
                            match result with
                            | Expectation.RemainNotSearchable ->
                                invalidArg (nameof codesResults) $"%s{nameof Expectation}.%s{nameof RemainNotSearchable} should be set for all codes of the item , given item %A{item}"
                            | Expectation.BeSearched hits -> { Text = code; Hits = hits }
                    ]

                    SearchSegment.Searched(SegmentId.Code, (SearchedSegmentTexts segmentChunks).Normalize(sortChunks = false), len)

            {
                Item = item
                Index = -1
                Segments = [ // ↩
                    searchSegment numResult SegmentId.Number item.Num
                    codeSearchSegment
                    searchSegment labelResult SegmentId.Hint item.Label
                ]
            }

[<AutoOpen>]
module ``2a_ run - normal search on prefixes - helpers`` =
    let AllPrefixItems =
        Map [
            for index, (PrefixItem item as prefix) in Seq.indexed Prefix.All do
                prefix, { item with Index = index }
        ]

    type Fixture(len) =
        member fixture.SearchSegment(prefix, ?num, ?code, ?label) =
            AllPrefixItems[prefix].ToSearchItem(len, numResult = num, codeResult = code, codesResults = None, labelResult = label, defaultExpectation = Expectation.RemainNotSearchable)

    [<RequireQualifiedAccess>]
    type InputForPrefixSearch =
        | D
        | F
        | Or
        | Re
        | T

    let (|PrefixSearchByCode|) (input, inputCase, searchOperation) =
        let searchInput = // ↩
            inputCase |> applyTo input |> SearchInput.create

        let searchType =
            match searchOperation with
            | SearchOperation.StartsWith -> Search.ByCodeStart
            | SearchOperation.Contains -> Search.ByCodeContent

        let fixture = Fixture(searchInput.Value.Length)

        let result =
            match input, searchOperation with
            // D: single-hit
            | InputForPrefixSearch.D, _ -> SearchableList.autoIndex [ fixture.SearchSegment(Prefix.Docs, code = "docs".ShouldMatchAtTheStart) ]

            // F: several single-hits
            | InputForPrefixSearch.F, SearchOperation.StartsWith ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Prefix.Feat, code = "feat".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Prefix.Fix, code = "fix".ShouldMatchAtTheStart)
                ]

            | InputForPrefixSearch.F, SearchOperation.Contains ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(Prefix.Feat, code = "feat".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Prefix.Fix, code = "fix".ShouldMatchAtTheStart)
                    fixture.SearchSegment(
                        Prefix.Refactor,
                        // . = "0123456789"
                        // . = "  f       "
                        code = "refactor".ShouldMatchAt(2)
                    )
                    fixture.SearchSegment(
                        Prefix.Perf,
                        // . = "0123456789"
                        // . = "   f      "
                        code = "perf".ShouldMatchAt(3)
                    )
                ]

            // T: double-hit (for `Test` + `Contains`)
            | InputForPrefixSearch.T, SearchOperation.StartsWith ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Prefix.Test, code = "test".ShouldMatchAtTheStart)
                ]

            | InputForPrefixSearch.T, SearchOperation.Contains ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(
                        Prefix.Feat,
                        // . = "0123456789"
                        // . = "   t      "
                        code = "feat".ShouldMatchAt(3)
                    )
                    fixture.SearchSegment(
                        Prefix.Refactor,
                        // . = "0123456789"
                        // . = "     t    "
                        code = "refactor".ShouldMatchAt(5)
                    )
                    fixture.SearchSegment(
                        Prefix.Test,
                        // . = "0123456789"
                        // . = "t  t      "
                        code = "test".ShouldMatchAt(0, 3)
                    )
                    fixture.SearchSegment(
                        Prefix.Revert,
                        // . = "0123456789"
                        // . = "     t    "
                        code = "revert".ShouldMatchAt(5)
                    )
                ]

            // Re: 2-char search, several single-hits
            | InputForPrefixSearch.Re, SearchOperation.StartsWith ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Prefix.Refactor, code = "refactor".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Prefix.Revert, code = "revert".ShouldMatchAtTheStart)
                ]

            | InputForPrefixSearch.Re, SearchOperation.Contains ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(Prefix.Refactor, code = "refactor".ShouldMatchAtTheStart)
                    fixture.SearchSegment(
                        Prefix.Chore,
                        // . = "0123456789"
                        // . = "   re     "
                        code = "chore".ShouldMatchAt(3)
                    )
                    fixture.SearchSegment(Prefix.Revert, code = "revert".ShouldMatchAtTheStart)
                ]

            // Or: no hit (for `StartsWith`)
            | InputForPrefixSearch.Or, SearchOperation.StartsWith -> []
            | InputForPrefixSearch.Or, SearchOperation.Contains ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(
                        Prefix.Refactor,
                        // . = "0123456789"
                        // . = "      or  "
                        code = "refactor".ShouldMatchAt(6)
                    )
                    fixture.SearchSegment(
                        Prefix.Chore,
                        // . = "0123456789"
                        // . = "  or      "
                        code = "chore".ShouldMatchAt(2)
                    )
                ]

        let items = AllPrefixItems |> Map.valuesAsList

        items, searchInput, searchType, result

module ``2a_ run - normal search on prefixes`` =
    [<Property>]
    let ``code only`` (PrefixSearchByCode(items, searchInput, searchType, result)) = // ↩
        let actual =
            Search(initSegmentsByIndex Segments.All searchType).Run(searchInput, items, StringComparison.OrdinalIgnoreCase)

        actual =! result

    [<Property>]
    let ``number exact match`` prefix = // ↩
        let item = AllPrefixItems[prefix]
        let items = AllPrefixItems |> Map.valuesAsList
        let fixture = Fixture(item.Num.Length)

        let actual =
            Search(initSegmentsByIndex Segments.All Search.ByNum).Run(SearchInput.create item.Num, items, StringComparison.OrdinalIgnoreCase)

        actual =! SearchableList.autoIndex [ fixture.SearchSegment(prefix, num = item.Num.ShouldMatchAtTheStart) ]

[<AutoOpen>]
module ``2b_ run - full-text search on emojis - helpers`` =
    let AllEmojiItems =
        Map [
            for index, (EmojiItem item as emoji) in Seq.indexed Emoji.All do
                emoji, { item with Index = index }
        ]

    type Fixture(len) =
        member fixture.SearchSegment(emoji, ?num, ?code, ?codes, ?label) =
            AllEmojiItems[emoji].ToSearchItem(len, numResult = num, codeResult = code, codesResults = codes, labelResult = label, defaultExpectation = Expectation.NotMatch)

    [<RequireQualifiedAccess>]
    type InputForEmojiSearch =
        | _2
        | _9
        | _10
        | Build
        | Down
        | Fix
        | Light
        | Test
        | Tick

    let (|EmojiFullTextSearch|) (input, inputCase) =
        let searchInput = // ↩
            inputCase |> applyTo input |> SearchInput.create

        let fixture = Fixture(searchInput.Value.Length)

        let result =
            match input with
            | InputForEmojiSearch._2 ->
                // 💡 Match the emoji's number starting by 2
                SearchableList.autoIndex [
                    fixture.SearchSegment(Emoji.Airplane, num = "2".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.CardFileBox, num = "20".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.ChartWithUpwardsTrend, num = "21".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.CheckMark, num = "22".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.ChildrenCrossing, num = "23".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.ClosedLockWithKey, num = "24".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.ClownFace, num = "25".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.Coffin, num = "26".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.Construction, num = "27".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.ConstructionWorker, num = "28".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.Dizzy, num = "29".ShouldMatchAtTheStart)
                ]

            | InputForEmojiSearch._9 ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Emoji.Beers, num = "9".ShouldMatchAtTheStart)
                ]

            | InputForEmojiSearch._10 ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Emoji.Bento, num = "10".ShouldMatchAtTheStart)
                ]

            | InputForEmojiSearch.Build ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(Emoji.BuildingConstruction, code = "building_construction".ShouldMatchAtTheStart)
                    fixture.SearchSegment(
                        Emoji.ConstructionWorker,
                        // .. = "0.........1.........2.........3.........4.........5........"
                        // .. = "01234567890123456789012345678901234567890123456789012345678"
                        // .. = "                 build                                     "
                        label = "Add or update CI build system.".ShouldMatchAt(17)
                    )
                    fixture.SearchSegment(
                        Emoji.GreenHeart,
                        // .. = "0.........1.........2.........3.........4.........5........"
                        // .. = "01234567890123456789012345678901234567890123456789012345678"
                        // .. = "       build                                               "
                        label = "Fix CI Build.".ShouldMatchAt(7)
                    )
                ]

            | InputForEmojiSearch.Down ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(
                        Emoji.ArrowDown, // ↩
                        code = "arrow_down".ShouldMatchAt(6),
                        label = "Downgrade dependencies.".ShouldMatchAtTheStart
                    )
                ]

            | InputForEmojiSearch.Fix ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(
                        Emoji.AdhesiveBandage,
                        // .. = "0.........1.........2.........3.........4.........5........"
                        // .. = "01234567890123456789012345678901234567890123456789012345678"
                        // .. = "       fix                                                 "
                        label = "Simple fix for a non-critical issue.".ShouldMatchAt(7)
                    )
                    fixture.SearchSegment(
                        Emoji.Ambulance,
                        // .. = "0.........1.........2.........3.........4.........5........"
                        // .. = "01234567890123456789012345678901234567890123456789012345678"
                        // .. = "            fix                                            "
                        label = "Critical hotfix.".ShouldMatchAt(12)
                    )
                    fixture.SearchSegment(Emoji.Bug, label = "Fix a bug.".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.GreenHeart, label = "Fix CI Build.".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.Lock, label = "Fix security or privacy issues.".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.Pencil, label = "Fix typos.".ShouldMatchAtTheStart)
                    fixture.SearchSegment(Emoji.RotatingLight, label = "Fix compiler / linter warnings.".ShouldMatchAtTheStart)
                ]

            | InputForEmojiSearch.Light ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(
                        Emoji.Bulb,
                        codes = [ // ↩
                            "light_bulb".ShouldMatchAtTheStart
                            "bulb".ShouldNotMatch
                            "idea".ShouldNotMatch
                        ]
                    )
                    fixture.SearchSegment(
                        Emoji.RotatingLight,
                        codes = [ // ↩
                            //.........1.........2.........3..
                            //12345678901234567890123456789012
                            "rotating_light".ShouldMatchAt(9)
                            "emergency_light".ShouldMatchAt(10)
                            "flashing_light".ShouldMatchAt(9)
                            "police_car_light".ShouldMatchAt(11)
                            "siren".ShouldNotMatch
                        ]
                    )
                    fixture.SearchSegment(
                        Emoji.Zap,
                        codes = [ // ↩
                            "lightning_bolt".ShouldMatchAtTheStart
                            "zap".ShouldNotMatch
                            "high_voltage".ShouldNotMatch
                            "thunderbolt".ShouldNotMatch
                        ]
                    )
                ]

            | InputForEmojiSearch.Test ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(
                        Emoji.CheckMark,
                        // .. = "0.........1.........2.........3.........4.........5........"
                        // .. = "01234567890123456789012345678901234567890123456789012345678"
                        // .. = "                     test                                  "
                        label = "Add, update, or pass tests.".ShouldMatchAt(21))
                    fixture.SearchSegment(
                        Emoji.TestTube,
                        code = "test_tube".ShouldMatchAtTheStart,
                        // .. = "0.........1.........2.........3.........4.........5........"
                        // .. = "01234567890123456789012345678901234567890123456789012345678"
                        // .. = "                test                                       "
                        label = "Add a (failing) test.".ShouldMatchAt(16))
                ]

            | InputForEmojiSearch.Tick ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(
                        Emoji.CheckMark,
                        codes = [
                            //.........1.........2.........3..
                            //12345678901234567890123456789012
                            "green_tick".ShouldMatchAt(6)
                            "check_mark".ShouldNotMatch
                            "white_check_mark".ShouldNotMatch
                        ]
                    )
                    fixture.SearchSegment(
                        Emoji.Lipstick,
                        codes = [
                            //.........1.........2.........3..
                            //12345678901234567890123456789012
                            "lipstick".ShouldMatchAt(4)
                        ])
                ]

        let items = AllEmojiItems |> Map.valuesAsList
        let searchInput = inputCase |> applyTo input |> SearchInput.create

        items, searchInput, result

module ``2b_ run - full-text search on emojis`` =
    [<Property>]
    let ``full-text search`` (EmojiFullTextSearch(items, searchInput, result)) = // ↩
        let actual =
            Search(initSegmentsByIndex Segments.All Search.FullText).Run(searchInput, items, StringComparison.OrdinalIgnoreCase)

        actual =! result