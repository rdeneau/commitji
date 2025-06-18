module Commitji.Core.Tests.SearchShould

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
    type SegmentType =
        | Num
        | Code
        | Label

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
            | SegmentType.Num -> item.Num
            | SegmentType.Code -> item.Code
            | SegmentType.Label -> item.Label

        member item.ToSearchSegment(segmentType, state) = {
            Id = segmentType
            Text = item.ToSegmentText(segmentType)
            State = state
        }

        member item.ToSegmentHit(segmentType, hits) =
            item.ToSearchSegment(segmentType, SegmentStateAfterSearch.Searched hits)

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

    type SearchSegments<'state when 'state: equality> = {
        Item: Item
        NumState: 'state option
        CodeState: 'state option
        LabelState: 'state option
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

        member this.AllNotSearchable() =
            this.All(SegmentInitState.NotSearchable)

        member this.ToList() = [
            match this.NumState with
            | Some state -> this.Item.ToSearchSegment(SegmentType.Num, state)
            | None -> ()

            match this.CodeState with
            | Some state -> this.Item.ToSearchSegment(SegmentType.Code, state)
            | None -> ()

            match this.LabelState with
            | Some state -> this.Item.ToSearchSegment(SegmentType.Label, state)
            | None -> ()
        ]

    let initSegmentsByIndex segments searchType =
        let finalizeBuild (searchSegments: SearchSegments<SegmentInitState>) =
            match segments, searchType with
            | Segments.CodeOnly, (Search.ByNum | Search.ByLabelContent) -> // ↩
                searchSegments.WithCodeState(SegmentInitState.NotSearchable).ToList()

            | Segments.CodeOnly, Search.ByCodeStart -> // ↩
                searchSegments.WithCodeState(SegmentInitState.Searchable SearchOperation.StartsWith).ToList()

            | Segments.CodeOnly, Search.ByCodeContent -> // ↩
                searchSegments.WithCodeState(SegmentInitState.Searchable SearchOperation.Contains).ToList()

            | Segments.CodeOnly, Search.FullText -> // ↩
                searchSegments.WithCodeState(SegmentInitState.Searchable SearchOperation.Contains).ToList()

            | Segments.All, Search.ByNum -> // ↩
                searchSegments.AllNotSearchable().WithNumState(SegmentInitState.Searchable SearchOperation.StartsWith).ToList()

            | Segments.All, Search.ByCodeStart -> // ↩
                searchSegments.AllNotSearchable().WithCodeState(SegmentInitState.Searchable SearchOperation.StartsWith).ToList()

            | Segments.All, Search.ByCodeContent -> // ↩
                searchSegments.AllNotSearchable().WithCodeState(SegmentInitState.Searchable SearchOperation.Contains).ToList()

            | Segments.All, Search.ByLabelContent -> // ↩
                searchSegments.AllNotSearchable().WithLabelState(SegmentInitState.Searchable SearchOperation.Contains).ToList()

            | Segments.All, Search.FullText ->
                searchSegments // ↩
                    .WithNumState(SegmentInitState.Searchable SearchOperation.StartsWith)
                    .WithCodeState(SegmentInitState.Searchable SearchOperation.Contains)
                    .WithLabelState(SegmentInitState.Searchable SearchOperation.Contains)
                    .ToList()

        fun _index (item: Item) ->
            let searchSegments = SearchSegments<SegmentInitState>.Init(item)
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
                            Id = SegmentType.Code
                            Text = item.Code
                            State =
                                match searchType with
                                | Search.ByNum
                                | Search.ByLabelContent -> SegmentInitState.NotSearchable
                                | Search.ByCodeStart -> SegmentInitState.Searchable SearchOperation.StartsWith
                                | Search.ByCodeContent
                                | Search.FullText -> SegmentInitState.Searchable SearchOperation.Contains
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
                            Id = SegmentType.Num
                            Text = item.Num
                            State =
                                match searchType with
                                | Search.ByNum
                                | Search.FullText -> SegmentInitState.Searchable SearchOperation.StartsWith
                                | Search.ByLabelContent
                                | Search.ByCodeStart
                                | Search.ByCodeContent -> SegmentInitState.NotSearchable
                        }
                        {
                            Id = SegmentType.Code
                            Text = item.Code
                            State =
                                match searchType with
                                | Search.ByNum
                                | Search.ByLabelContent -> SegmentInitState.NotSearchable
                                | Search.ByCodeStart -> SegmentInitState.Searchable SearchOperation.StartsWith
                                | Search.ByCodeContent
                                | Search.FullText -> SegmentInitState.Searchable SearchOperation.Contains
                        }
                        {
                            Id = SegmentType.Label
                            Text = item.Label
                            State =
                                match searchType with
                                | Search.FullText
                                | Search.ByLabelContent -> SegmentInitState.Searchable SearchOperation.Contains
                                | Search.ByCodeStart
                                | Search.ByCodeContent
                                | Search.ByNum -> SegmentInitState.NotSearchable
                        }
                    ]
                }
        ]

        let actual = Search(initSegmentsByIndex Segments.All searchType).Init(items)

        actual =! expected

[<AutoOpen>]
module ``2_ run - common - helpers`` =
    [<RequireQualifiedAccess>]
    module SearchedList =
        let autoIndex (list: SearchedList<'t, 'id>) =
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
    module Is =
        let notSearchable = SegmentStateAfterSearch.NotSearchable
        let notFound = SegmentStateAfterSearch.Searched []
        let foundAtTheStart = SegmentStateAfterSearch.Searched [ 0 ]
        let foundOnceAt hit = SegmentStateAfterSearch.Searched [ hit ]
        let foundTwiceAt hit1 hit2 = SegmentStateAfterSearch.Searched [ hit1; hit2 ]

    type Item with
        /// <remarks>
        /// ⚠️ The `Index` is set to -1 → Call `SearchedList.autoIndex` on the list to set the correct index.<br/>
        /// 💡 For `defaultValue`, use `Is.notSearchable` (normal search) or `Is.notFound` (full-text search).<br/>
        /// </remarks>
        member item.ToSearchSegment(num, code, label, defaultValue) = {
            Item = item
            Index = -1
            Segments = [ // ↩
                item.ToSearchSegment(SegmentType.Num, defaultArg num defaultValue)
                item.ToSearchSegment(SegmentType.Code, defaultArg code defaultValue)
                item.ToSearchSegment(SegmentType.Label, defaultArg label defaultValue)
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

    type Prefix with
        member prefix.ToSearchSegment(?num, ?code, ?label) =
            AllPrefixItems[prefix].ToSearchSegment(num, code, label, defaultValue = Is.notSearchable)

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

        let result =
            match input, searchOperation with
            // D: single-hit
            | InputForPrefixSearch.D, SearchOperation.StartsWith -> SearchedList.autoIndex [ Prefix.Docs.ToSearchSegment(code = Is.foundAtTheStart) ]
            | InputForPrefixSearch.D, SearchOperation.Contains -> SearchedList.autoIndex [ Prefix.Docs.ToSearchSegment(code = Is.foundAtTheStart) ]

            // F: several single-hits
            | InputForPrefixSearch.F, SearchOperation.StartsWith ->
                SearchedList.autoIndex [ // ↩
                    Prefix.Feat.ToSearchSegment(code = Is.foundAtTheStart)
                    Prefix.Fix.ToSearchSegment(code = Is.foundAtTheStart)
                ]

            | InputForPrefixSearch.F, SearchOperation.Contains ->
                SearchedList.autoIndex [
                    Prefix.Feat.ToSearchSegment(code = Is.foundAtTheStart)
                    Prefix.Fix.ToSearchSegment(code = Is.foundAtTheStart)

                    // ... 012345678
                    // ...   ↓
                    Prefix.Refactor.ToSearchSegment(code = Is.foundOnceAt 2)

                    // ... 012345678
                    // ...    ↓
                    Prefix.Perf.ToSearchSegment(code = Is.foundOnceAt 3)
                ]

            // T: double-hit (for `Test` + `Contains`)
            | InputForPrefixSearch.T, SearchOperation.StartsWith ->
                SearchedList.autoIndex [ // ↩
                    Prefix.Test.ToSearchSegment(code = Is.foundAtTheStart)
                ]

            | InputForPrefixSearch.T, SearchOperation.Contains ->
                SearchedList.autoIndex [
                    // ... 012345678
                    // ...    ↓
                    Prefix.Feat.ToSearchSegment(code = Is.foundOnceAt 3)

                    // ... 012345678
                    // ...      ↓
                    Prefix.Refactor.ToSearchSegment(code = Is.foundOnceAt 5)

                    // ... 012345678
                    // ... ↓  ↓
                    Prefix.Test.ToSearchSegment(code = Is.foundTwiceAt 0 3)

                    // ... 012345678
                    // ...      ↓
                    Prefix.Revert.ToSearchSegment(code = Is.foundOnceAt 5)
                ]

            // Re: 2-char search, several single-hits
            | InputForPrefixSearch.Re, SearchOperation.StartsWith ->
                SearchedList.autoIndex [ // ↩
                    Prefix.Refactor.ToSearchSegment(code = Is.foundAtTheStart)
                    Prefix.Revert.ToSearchSegment(code = Is.foundAtTheStart)
                ]

            | InputForPrefixSearch.Re, SearchOperation.Contains ->
                SearchedList.autoIndex [
                    Prefix.Refactor.ToSearchSegment(code = Is.foundAtTheStart)

                    // ... 012345678
                    // ...    ↓
                    Prefix.Chore.ToSearchSegment(code = Is.foundOnceAt 3)

                    Prefix.Revert.ToSearchSegment(code = Is.foundAtTheStart)
                ]

            // Or: no hit (for `StartsWith`)
            | InputForPrefixSearch.Or, SearchOperation.StartsWith -> []
            | InputForPrefixSearch.Or, SearchOperation.Contains ->
                SearchedList.autoIndex [
                    // ... 012345678
                    // ...       ↓
                    Prefix.Refactor.ToSearchSegment(code = Is.foundOnceAt 6)

                    // ... 012345678
                    // ...   ↓
                    Prefix.Chore.ToSearchSegment(code = Is.foundOnceAt 2)
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

        let actual =
            Search(initSegmentsByIndex Segments.All Search.ByNum).Run(SearchInput.create item.Num, items, StringComparison.OrdinalIgnoreCase)

        actual =! SearchedList.autoIndex [ (prefix.ToSearchSegment(num = Is.foundAtTheStart)) ]

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

    type Emoji with
        member emoji.ToSearchSegment(?num, ?code, ?label) =
            AllEmojiItems[emoji].ToSearchSegment(num, code, label, defaultValue = Is.notFound)

    [<RequireQualifiedAccess>]
    type InputForEmojiSearch =
        | _2
        | Build
        | Down
        | Fix
        | Test

    let (|EmojiFullTextSearch|) (input, inputCase) =
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
                SearchedList.autoIndex [
                    Emoji.Airplane.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.ChartWithUpwardsTrend.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.ChildrenCrossing.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.ClosedLockWithKey.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.ClownFace.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.Coffin.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.Construction.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.ConstructionWorker.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.Dizzy.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.Egg.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.Fire.ToSearchSegment(num = Is.foundAtTheStart)
                    Emoji.Pencil2.ToSearchSegment(code = Is.foundOnceAt 6)
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
                SearchedList.autoIndex [ // ↩
                    Emoji.BuildingConstruction.ToSearchSegment(code = Is.foundAtTheStart)
                    Emoji.ConstructionWorker.ToSearchSegment(label = Is.foundOnceAt 17)
                    Emoji.GreenHeart.ToSearchSegment(label = Is.foundOnceAt 7)
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
                SearchedList.autoIndex [ // ↩
                    Emoji.ArrowDown.ToSearchSegment(code = Is.foundOnceAt 6, label = Is.foundAtTheStart)
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
                SearchedList.autoIndex [
                    Emoji.AdhesiveBandage.ToSearchSegment(label = Is.foundOnceAt 7)
                    Emoji.Ambulance.ToSearchSegment(label = Is.foundOnceAt 12)
                    Emoji.Bug.ToSearchSegment(label = Is.foundAtTheStart)
                    Emoji.GreenHeart.ToSearchSegment(label = Is.foundAtTheStart)
                    Emoji.Lock.ToSearchSegment(label = Is.foundAtTheStart)
                    Emoji.Pencil2.ToSearchSegment(label = Is.foundAtTheStart)
                    Emoji.RotatingLight.ToSearchSegment(label = Is.foundAtTheStart)
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
                SearchedList.autoIndex [ // ↩
                    Emoji.TestTube.ToSearchSegment(code = Is.foundAtTheStart, label = Is.foundOnceAt 14)
                    Emoji.WhiteCheckMark.ToSearchSegment(label = Is.foundOnceAt 21)
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