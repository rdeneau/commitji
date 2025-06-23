module Commitji.Tests.Core.SearchShould

open System
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
        Code: string
        Label: string
        Index: int
    } with
        member item.Num = $"%i{item.Index + 1}"

        member item.ToSegmentText(segmentType) =
            match segmentType with
            | SegmentId.Number -> SegmentText item.Num
            | SegmentId.Code -> SegmentText item.Code
            | SegmentId.Hint -> SegmentText item.Label

        member item.ToSearchSegment(segmentType, state) = {
            Id = segmentType
            Text = item.ToSegmentText(segmentType)
            State = state
        }

        member item.ToSegmentHit(segmentType, hits) =
            item.ToSearchSegment(segmentType, SegmentState.Searched hits)

    let (|PrefixItem|) (prefix: Prefix) = {
        Type = ItemType.Prefix prefix
        Code = prefix.Code
        Label = prefix.Hint
        Index = 0
    }

    let (|EmojiItem|) (emoji: Emoji) = {
        Type = ItemType.Emoji emoji
        Code = emoji.Code
        Label = emoji.Hint
        Index = 0
    }

    let (|BreakingChangeItem|) (breakingChange: bool) = {
        Type = ItemType.BreakingChange breakingChange
        Code = if breakingChange then "Yes" else "No"
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
        NumState: SegmentState option
        CodeState: SegmentState option
        LabelState: SegmentState option
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

        member this.AllNotSearchable() = this.All(SegmentState.NotSearchable)

        member this.ToList() = [
            match this.NumState with
            | Some state -> this.Item.ToSearchSegment(SegmentId.Number, state)
            | None -> ()

            match this.CodeState with
            | Some state -> this.Item.ToSearchSegment(SegmentId.Code, state)
            | None -> ()

            match this.LabelState with
            | Some state -> this.Item.ToSearchSegment(SegmentId.Hint, state)
            | None -> ()
        ]

    let initSegmentsByIndex segments searchType =
        let finalizeBuild (searchSegments: SearchSegments) =
            match segments, searchType with
            | Segments.CodeOnly, (Search.ByNum | Search.ByLabelContent) -> // ↩
                searchSegments.WithCodeState(SegmentState.NotSearchable).ToList()

            | Segments.CodeOnly, Search.ByCodeStart -> // ↩
                searchSegments.WithCodeState(SegmentState.Searchable SearchOperation.StartsWith).ToList()

            | Segments.CodeOnly, Search.ByCodeContent -> // ↩
                searchSegments.WithCodeState(SegmentState.Searchable SearchOperation.Contains).ToList()

            | Segments.CodeOnly, Search.FullText -> // ↩
                searchSegments.WithCodeState(SegmentState.Searchable SearchOperation.Contains).ToList()

            | Segments.All, Search.ByNum -> // ↩
                searchSegments.AllNotSearchable().WithNumState(SegmentState.Searchable SearchOperation.StartsWith).ToList()

            | Segments.All, Search.ByCodeStart -> // ↩
                searchSegments.AllNotSearchable().WithCodeState(SegmentState.Searchable SearchOperation.StartsWith).ToList()

            | Segments.All, Search.ByCodeContent -> // ↩
                searchSegments.AllNotSearchable().WithCodeState(SegmentState.Searchable SearchOperation.Contains).ToList()

            | Segments.All, Search.ByLabelContent -> // ↩
                searchSegments.AllNotSearchable().WithLabelState(SegmentState.Searchable SearchOperation.Contains).ToList()

            | Segments.All, Search.FullText ->
                searchSegments // ↩
                    .WithNumState(SegmentState.Searchable SearchOperation.StartsWith)
                    .WithCodeState(SegmentState.Searchable SearchOperation.Contains)
                    .WithLabelState(SegmentState.Searchable SearchOperation.Contains)
                    .ToList()

        fun _index (item: Item) ->
            let searchSegments = SearchSegments.Init(item)
            finalizeBuild searchSegments

module ``1_ init`` =
    [<Property>]
    let ``code only segment`` (ManyItems items) searchType = // ↩
        let expected = [
            for index, item in List.indexed items do
                {
                    Item = item
                    Index = index
                    Segments = [
                        {
                            Id = SegmentId.Code
                            Text = SegmentText item.Code
                            State =
                                match searchType with
                                | Search.ByNum
                                | Search.ByLabelContent -> SegmentState.NotSearchable
                                | Search.ByCodeStart -> SegmentState.Searchable SearchOperation.StartsWith
                                | Search.ByCodeContent
                                | Search.FullText -> SegmentState.Searchable SearchOperation.Contains
                        }
                    ]
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
                        {
                            Id = SegmentId.Number
                            Text = SegmentText item.Num
                            State =
                                match searchType with
                                | Search.ByNum
                                | Search.FullText -> SegmentState.Searchable SearchOperation.StartsWith
                                | Search.ByLabelContent
                                | Search.ByCodeStart
                                | Search.ByCodeContent -> SegmentState.NotSearchable
                        }
                        {
                            Id = SegmentId.Code
                            Text = SegmentText item.Code
                            State =
                                match searchType with
                                | Search.ByNum
                                | Search.ByLabelContent -> SegmentState.NotSearchable
                                | Search.ByCodeStart -> SegmentState.Searchable SearchOperation.StartsWith
                                | Search.ByCodeContent
                                | Search.FullText -> SegmentState.Searchable SearchOperation.Contains
                        }
                        {
                            Id = SegmentId.Hint
                            Text = SegmentText item.Label
                            State =
                                match searchType with
                                | Search.FullText
                                | Search.ByLabelContent -> SegmentState.Searchable SearchOperation.Contains
                                | Search.ByCodeStart
                                | Search.ByCodeContent
                                | Search.ByNum -> SegmentState.NotSearchable
                        }
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

    [<RequireQualifiedAccess>]
    type Fixture(len) =
        member _.NotSearchable = SegmentState.NotSearchable
        member _.NotFound = SegmentState.Searched([], len)

        member _.FoundAt([<ParamArray>] hits) =
            SegmentState.Searched(List.ofArray hits, len)

        member x.FoundAtTheStart = x.FoundAt(0)
        member x.FoundOnceAt(i: int) = x.FoundAt(i)
        member x.FoundTwiceAt(i, j) = x.FoundAt(i, j)

    type Item with
        /// <remarks>
        /// ⚠️ The `Index` is set to -1 → Call `SearchableList.autoIndex` on the list to set the correct index.<br/>
        /// 💡 For `defaultValue`, use `_.notSearchable` (normal search) or `_.notFound` (full-text search).<br/>
        /// </remarks>
        member item.ToSearchSegment(fixture: Fixture, num, code, label, defaultValue) = {
            Item = item
            Index = -1
            Segments = [ // ↩
                item.ToSearchSegment(SegmentId.Number, (defaultArg num defaultValue) fixture)
                item.ToSearchSegment(SegmentId.Code, (defaultArg code defaultValue) fixture)
                item.ToSearchSegment(SegmentId.Hint, (defaultArg label defaultValue) fixture)
            ]
        }

[<AutoOpen>]
module ``2a_ run - normal search on prefixes - helpers`` =
    // | Num | Code     | Label                                                           |
    // |-----|----------|-----------------------------------------------------------------|
    // | -   | 01234567 | 012345678901234567890123456789012345678901234567890123456789012 |
    // | --  | 0....... | 0.........1.........2.........3.........4.........5.........6.. |
    // |-----|----------|-----------------------------------------------------------------|
    // |  1  | feat     | A new feature                                                   |
    // |  2  | fix      | A bug fix                                                       |
    // |  3  | refactor | A code change that does not alter the functionality             |
    // |  4  | test     | Adding missing tests                                            |
    // |  5  | chore    | Any other changes finalized: config, build, ci, dependencies... |
    // |  6  | docs     | Documentation only changes                                      |
    // |  7  | perf     | A code change that improves performance                         |
    // |  8  | revert   | Reverts a previous commit                                       |
    // |  9  | wip      | Work in progress, not yet finalized                             |

    let AllPrefixItems =
        Map [
            for index, (PrefixItem item as prefix) in Seq.indexed Prefix.All do
                prefix, { item with Index = index }
        ]

    type Fixture with
        member this.SearchSegment(prefix: Prefix, ?num, ?code, ?label) =
            AllPrefixItems[prefix].ToSearchSegment(this, num, code, label, defaultValue = _.NotSearchable)

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
            | InputForPrefixSearch.D, SearchOperation.StartsWith -> SearchableList.autoIndex [ fixture.SearchSegment(Prefix.Docs, code = _.FoundAtTheStart) ]
            | InputForPrefixSearch.D, SearchOperation.Contains -> SearchableList.autoIndex [ fixture.SearchSegment(Prefix.Docs, code = _.FoundAtTheStart) ]

            // F: several single-hits
            | InputForPrefixSearch.F, SearchOperation.StartsWith ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Prefix.Feat, code = _.FoundAtTheStart)
                    fixture.SearchSegment(Prefix.Fix, code = _.FoundAtTheStart)
                ]

            | InputForPrefixSearch.F, SearchOperation.Contains ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(Prefix.Feat, code = _.FoundAtTheStart)
                    fixture.SearchSegment(Prefix.Fix, code = _.FoundAtTheStart)

                    // ... 012345678
                    // ...   ↓
                    fixture.SearchSegment(Prefix.Refactor, code = _.FoundOnceAt(2))

                    // ... 012345678
                    // ...    ↓
                    fixture.SearchSegment(Prefix.Perf, code = _.FoundOnceAt(3))
                ]

            // T: double-hit (for `Test` + `Contains`)
            | InputForPrefixSearch.T, SearchOperation.StartsWith ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Prefix.Test, code = _.FoundAtTheStart)
                ]

            | InputForPrefixSearch.T, SearchOperation.Contains ->
                SearchableList.autoIndex [
                    // ... 012345678
                    // ...    ↓
                    fixture.SearchSegment(Prefix.Feat, code = _.FoundOnceAt(3))

                    // ... 012345678
                    // ...      ↓
                    fixture.SearchSegment(Prefix.Refactor, code = _.FoundOnceAt(5))

                    // ... 012345678
                    // ... ↓  ↓
                    fixture.SearchSegment(Prefix.Test, code = _.FoundTwiceAt(0, 3))

                    // ... 012345678
                    // ...      ↓
                    fixture.SearchSegment(Prefix.Revert, code = _.FoundOnceAt(5))
                ]

            // Re: 2-char search, several single-hits
            | InputForPrefixSearch.Re, SearchOperation.StartsWith ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Prefix.Refactor, code = _.FoundAtTheStart)
                    fixture.SearchSegment(Prefix.Revert, code = _.FoundAtTheStart)
                ]

            | InputForPrefixSearch.Re, SearchOperation.Contains ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(Prefix.Refactor, code = _.FoundAtTheStart)

                    // ... 012345678
                    // ...    ↓
                    fixture.SearchSegment(Prefix.Chore, code = _.FoundOnceAt(3))

                    fixture.SearchSegment(Prefix.Revert, code = _.FoundAtTheStart)
                ]

            // Or: no hit (for `StartsWith`)
            | InputForPrefixSearch.Or, SearchOperation.StartsWith -> []
            | InputForPrefixSearch.Or, SearchOperation.Contains ->
                SearchableList.autoIndex [
                    // ... 012345678
                    // ...       ↓
                    fixture.SearchSegment(Prefix.Refactor, code = _.FoundOnceAt(6))

                    // ... 012345678
                    // ...   ↓
                    fixture.SearchSegment(Prefix.Chore, code = _.FoundOnceAt(2))
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

        actual =! SearchableList.autoIndex [ fixture.SearchSegment(prefix, num = _.FoundAtTheStart) ]

[<AutoOpen>]
module ``2b_ run - full-text search on emojis - helpers`` =
    // | Num | Code                      | Label                                                         |
    // |-----|---------------------------|---------------------------------------------------------------|
    // | -   | 0123456789012345678901234 | 0123456789012345678901234567890123456789012345678901234567890 |
    // | --  | 0.........1.........2.... | 0.........1.........2.........3.........4.........5.........6 |
    // |-----|---------------------------|---------------------------------------------------------------|
    // | 1   | adhesive_bandage          | Simple fix for a non-critical issue.                          |
    // | 2   | airplane                  | Improve offline support.                                      |
    // | 3   | alembic                   | Perform experiments.                                          |
    // | 4   | alien                     | Update code due to external API changes.                      |
    // | 5   | ambulance                 | Critical hotfix.                                              |
    // | 6   | arrow_down                | Downgrade dependencies.                                       |
    // | 7   | arrow_up                  | Upgrade dependencies.                                         |
    // | 8   | art                       | Improve structure / format of the code.                       |
    // | 9   | beers                     | Write code drunkenly.                                         |
    // | 10  | bento                     | Add or update assets.                                         |
    // | 11  | bookmark                  | Release / Version tags.                                       |
    // | 12  | boom                      | Introduce breaking changes.                                   |
    // | 13  | bricks                    | Infrastructure related changes.                               |
    // | 14  | bug                       | Fix a bug.                                                    |
    // | 15  | building_construction     | Make architectural changes.                                   |
    // | 16  | bulb                      | Add or update comments in source code.                        |
    // | 17  | busts_in_silhouette       | Add or update contributor(s).                                 |
    // | 18  | camera_flash              | Add or update snapshots.                                      |
    // | 19  | card_file_box             | Perform database related changes.                             |
    // | 20  | chart_with_upwards_trend  | Add or update analytics or track code.                        |
    // | 21  | children_crossing         | Improve user experience / usability.                          |
    // | 22  | closed_lock_with_key      | Add or update secrets.                                        |
    // | 23  | clown_face                | Mock things.                                                  |
    // | 24  | coffin                    | Remove dead code.                                             |
    // | 25  | construction              | Work in progress, not yet finalized.                          |
    // | 26  | construction_worker       | Add or update CI build system.                                |
    // | 27  | dizzy                     | Add or update animations and transitions.                     |
    // | 28  | egg                       | Add or update an easter egg.                                  |
    // | 29  | fire                      | Remove code or files.                                         |
    // | 30  | globe_with_meridians      | Internationalization and localization.                        |
    // | 31  | goal_net                  | Catch errors.                                                 |
    // | 32  | green_heart               | Fix CI Build.                                                 |
    // | 33  | hammer                    | Add or update development scripts.                            |
    // | 34  | heavy_minus_sign          | Remove a dependency.                                          |
    // | 35  | heavy_plus_sign           | Add a dependency.                                             |
    // | 36  | iphone                    | Work on responsive design.                                    |
    // | 37  | label                     | Add or update types.                                          |
    // | 38  | lipstick                  | Add or update the UI and style files.                         |
    // | 39  | lock                      | Fix security or privacy issues.                               |
    // | 40  | loud_sound                | Add or update logs.                                           |
    // | 41  | mag                       | Improve SEO.                                                  |
    // | 42  | memo                      | Add or update documentation.                                  |
    // | 43  | money_with_wings          | Add sponsorships or money related infrastructure.             |
    // | 44  | monocle_face              | Data exploration/inspection.                                  |
    // | 45  | mute                      | Remove logs.                                                  |
    // | 46  | necktie                   | Add or update business logic.                                 |
    // | 47  | package                   | Add or update compiled files or packages.                     |
    // | 48  | page_facing_up            | Add or update license.                                        |
    // | 49  | passport_control          | Work on code related to authorization, roles and permissions. |
    // | 50  | pencil2                   | Fix typos.                                                    |
    // | 51  | poop                      | Write bad code that needs to be improved.                     |
    // | 52  | pushpin                   | Pin dependencies to specific versions.                        |
    // | 53  | recycle                   | Refactor code: without changing its behavior.                 |
    // | 54  | rewind                    | Revert changes.                                               |
    // | 55  | rocket                    | Deploy stuff.                                                 |
    // | 56  | rotating_light            | Fix compiler / linter warnings.                               |
    // | 57  | safety_vest               | Add or update code related to validation.                     |
    // | 58  | seedling                  | Add or update seed files.                                     |
    // | 59  | see_no_evil               | Add or update a .gitignore file.                              |
    // | 60  | sparkles                  | Introduces a new feature.                                     |
    // | 61  | speech_balloon            | Add or update text and literals.                              |
    // | 62  | stethoscope               | Add or update healthcheck.                                    |
    // | 63  | tada                      | Begin a project.                                              |
    // | 64  | technologist              | Improve developer experience.                                 |
    // | 65  | test_tube                 | Add a failing test.                                           |
    // | 66  | thread                    | Add or update code related to multithreading or concurrency.  |
    // | 67  | triangular_flag_on_post   | Add, update, or remove feature flags.                         |
    // | 68  | truck                     | Move or rename resources (e.g.: files, paths, routes).        |
    // | 69  | twisted_rightwards_arrows | Merge branches.                                               |
    // | 70  | wastebasket               | Deprecate code that needs to be cleaned up.                   |
    // | 71  | wheelchair                | Improve accessibility.                                        |
    // | 72  | white_check_mark          | Add, update, or pass tests.                                   |
    // | 73  | wrench                    | Add or update configuration files.                            |
    // | 74  | zap                       | Improve performance.                                          |

    let AllEmojiItems =
        Map [
            for index, (EmojiItem item as emoji) in Seq.indexed Emoji.All do
                emoji, { item with Index = index }
        ]

    type Fixture with
        member this.SearchSegment(emoji: Emoji, ?num, ?code, ?label) =
            AllEmojiItems[emoji].ToSearchSegment(this, num, code, label, defaultValue = _.NotFound)

    [<RequireQualifiedAccess>]
    type InputForEmojiSearch =
        | _2
        | Build
        | Down
        | Fix
        | Test

    let (|EmojiFullTextSearch|) (input, inputCase) =
        let searchInput = // ↩
            inputCase |> applyTo input |> SearchInput.create

        let fixture = Fixture(searchInput.Value.Length)

        let result =
            match input with
            // | Num | Code                      | Label                                                         |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | -   | 0123456789012345678901234 | 0123456789012345678901234567890123456789012345678901234567890 |
            // | --  | 0.........1.........2.... | 0.........1.........2.........3.........4.........5.........6 |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 2   | airplane                  | Improve offline support.                                      |
            // | ↑   |                           |                                                               |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 20  | chart_with_upwards_trend  | Add or update analytics or track code.                        |
            // | 21  | children_crossing         | Improve user experience / usability.                          |
            // | 22  | closed_lock_with_key      | Add or update secrets.                                        |
            // | 23  | clown_face                | Mock things.                                                  |
            // | 24  | coffin                    | Remove dead code.                                             |
            // | 25  | construction              | Work in progress, not yet finalized.                          |
            // | 26  | construction_worker       | Add or update CI build system.                                |
            // | 27  | dizzy                     | Add or update animations and transitions.                     |
            // | 28  | egg                       | Add or update an easter egg.                                  |
            // | 29  | fire                      | Remove code or files.                                         |
            // | ↑   |                           |                                                               |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 50  | pencil2                   | Fix typos.                                                    |
            // |     |       ↑                   |                                                               |
            // |-----|---------------------------|---------------------------------------------------------------|
            | InputForEmojiSearch._2 ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(Emoji.Airplane, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.ChartWithUpwardsTrend, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.ChildrenCrossing, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.ClosedLockWithKey, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.ClownFace, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.Coffin, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.Construction, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.ConstructionWorker, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.Dizzy, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.Egg, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.Fire, num = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.Pencil2, code = _.FoundOnceAt(6))
                ]

            // | Num | Code                      | Label                                                         |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | -   | 0123456789012345678901234 | 0123456789012345678901234567890123456789012345678901234567890 |
            // | --  | 0.........1.........2.... | 0.........1.........2.........3.........4.........5.........6 |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 15  | building_construction     | Make architectural changes.                                   |
            // |     | ↑                         |                                                               |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 26  | construction_worker       | Add or update CI build system.                                |
            // |     |                           |                  ↑                                            |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 32  | green_heart               | Fix CI Build.                                                 |
            // |     |                           |        ↑                                                      |
            // |-----|---------------------------|---------------------------------------------------------------|
            | InputForEmojiSearch.Build ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Emoji.BuildingConstruction, code = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.ConstructionWorker, label = _.FoundOnceAt(17))
                    fixture.SearchSegment(Emoji.GreenHeart, label = _.FoundOnceAt(7))
                ]

            // | Num | Code                      | Label                                                         |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | -   | 0123456789012345678901234 | 0123456789012345678901234567890123456789012345678901234567890 |
            // | --  | 0.........1.........2.... | 0.........1.........2.........3.........4.........5.........6 |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 6   | arrow_down                | Downgrade dependencies.                                       |
            // |     |       ↑                   | ↑                                                             |
            // |-----|---------------------------|---------------------------------------------------------------|
            | InputForEmojiSearch.Down ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Emoji.ArrowDown, code = _.FoundOnceAt(6), label = _.FoundAtTheStart)
                ]

            // | Num | Code                      | Label                                                         |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | -   | 0123456789012345678901234 | 0123456789012345678901234567890123456789012345678901234567890 |
            // | --  | 0.........1.........2.... | 0.........1.........2.........3.........4.........5.........6 |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 1   | adhesive_bandage          | Simple fix for a non-critical issue.                          |
            // |     |                           |        ↑                                                      |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 5   | ambulance                 | Critical hotfix.                                              |
            // |     |                           |             ↑                                                 |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 14  | bug                       | Fix a bug.                                                    |
            // | 32  | green_heart               | Fix CI Build.                                                 |
            // | 39  | lock                      | Fix security or privacy issues.                               |
            // | 50  | pencil2                   | Fix typos.                                                    |
            // | 56  | rotating_light            | Fix compiler / linter warnings.                               |
            // |     |                           | ↑                                                             |
            // |-----|---------------------------|---------------------------------------------------------------|
            | InputForEmojiSearch.Fix ->
                SearchableList.autoIndex [
                    fixture.SearchSegment(Emoji.AdhesiveBandage, label = _.FoundOnceAt(7))
                    fixture.SearchSegment(Emoji.Ambulance, label = _.FoundOnceAt(12))
                    fixture.SearchSegment(Emoji.Bug, label = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.GreenHeart, label = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.Lock, label = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.Pencil2, label = _.FoundAtTheStart)
                    fixture.SearchSegment(Emoji.RotatingLight, label = _.FoundAtTheStart)
                ]

            // | Num | Code                      | Label                                                         |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | -   | 0123456789012345678901234 | 0123456789012345678901234567890123456789012345678901234567890 |
            // | --  | 0.........1.........2.... | 0.........1.........2.........3.........4.........5.........6 |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 65  | test_tube                 | Add a failing test.                                           |
            // |     | ↑                         |               ↑                                               |
            // |-----|---------------------------|---------------------------------------------------------------|
            // | 72  | white_check_mark          | Add, update, or pass tests.                                   |
            // |     |                           |                      ↑                                        |
            // |-----|---------------------------|---------------------------------------------------------------|
            | InputForEmojiSearch.Test ->
                SearchableList.autoIndex [ // ↩
                    fixture.SearchSegment(Emoji.TestTube, code = _.FoundAtTheStart, label = _.FoundOnceAt(14))
                    fixture.SearchSegment(Emoji.WhiteCheckMark, label = _.FoundOnceAt(21))
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