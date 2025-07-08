module Commitji.Tests.Core.StateShould

open Commitji.Core.Model
open Commitji.Core.Model.Search
open Commitji.Core.State
open Commitji.Core.Types
open FsCheck.Xunit
open Swensen.Unquote
open global.Xunit

[<AutoOpen>]
module Search =
    /// Simplified search mode for testing purposes:
    /// the searchable segment is limited to the code of the prefix or emoji.
    type SearchBy =
        // TODO: 💡 decorate `label` with `[<CallerArgumentExpression("operation"); Optional>]` whenever it's supported in F#.
        static member private CodeOnly(operation, label) =
            SearchMode.Custom(
                label,
                {
                    SegmentsConfiguration.States = // ↩
                        Map [ SegmentId.Code, SegmentConfig.Searchable operation ]
                }
            )

        static member PrefixCodeStartingWith =
            SearchBy.CodeOnly(SearchOperation.StartsWith, nameof SearchBy.PrefixCodeStartingWith)

        static member EmojiCodeContaining =
            SearchBy.CodeOnly(SearchOperation.Contains, nameof SearchBy.EmojiCodeContaining)

    type SearchMode with
        member this.SegmentsConfiguration =
            match this with
            | SearchMode.Quick
            | SearchMode.FullText -> failwith $"Invalid search mode: {this} not supported in unit tests."
            | SearchMode.Custom(_, segmentsConfiguration) -> segmentsConfiguration

        member this.SearchablePrefixes prefixes =
            SelectableList.Prefixes.searchable prefixes this.SegmentsConfiguration

        member this.SearchableEmojis emojis =
            SelectableList.Emojis.searchable emojis this.SegmentsConfiguration

    [<RequireQualifiedAccess>]
    module Searched =
        type CodeHits<'item>(getCode: 'item -> string, input: SearchInput) =
            let length = input.Length

            member _.SearchItemMultiCodes(index, item, codesWithHits) =
                let segments =
                    match codesWithHits with
                    | [ code, hits ] -> SearchedSegmentText { Text = code; Hits = hits }
                    | _ -> SearchedSegmentTexts [ for code, hits in codesWithHits -> { Text = code; Hits = hits } ]

                {
                    Item = item
                    Index = index
                    Segments = [ SearchSegment.Searched(SegmentId.Code, segments, length) ]
                }

            member this.SearchItem(index, item, hits) =
                this.SearchItemMultiCodes(index, item, [ getCode item, hits ])

        type CodeHitsSource<'item>(getCode: 'item -> string) =
            member _.By(input) =
                CodeHits(getCode, SearchInput.create input)

        let prefix = CodeHitsSource(fun (x: Prefix) -> x.Code)
        let emoji = CodeHitsSource(fun (x: Emoji) -> x.Code)

[<AutoOpen>]
module Fixture =
    type Fixture = { Model: Model }

    let private searchMode = SearchBy.PrefixCodeStartingWith

    let private initial =
        {
            CurrentStep = { Step = Step.Prefix(searchMode.SearchablePrefixes Prefix.All); Input = "" }
            CompletedSteps = []
            AvailablePrefixes = Prefix.All
            AvailableEmojis = Emoji.All
            AvailableBreakingChanges = BreakingChange.All
            AvailablePossibilities = []
            SearchMode = searchMode
            History = []
            Errors = []
        }
        |> definePossibilities

    let private initialForEmojis =
        let searchMode = SearchBy.EmojiCodeContaining

        { initial with CurrentStep = { Step = Step.Emoji(searchMode.SearchableEmojis Emoji.All); Input = "" }; SearchMode = searchMode }
        |> definePossibilities

    let private withHitAtStart items = [
        for item in items do
            item, [ 0 ]
    ]

    type Fixture with
        static member Initial = { Model = initial }
        static member InitialForEmojis = { Model = initialForEmojis }

        member private this.SearchMode = this.Model.SearchMode

        member this.SearchablePrefixes = this.SearchMode.SearchablePrefixes
        member this.SearchableEmojis = this.SearchMode.SearchableEmojis

        static member private SearchedItem(builder: Searched.CodeHits<_>, itemsWithHits) =
            SelectableList.init [
                for index, (item: 'item, hits) in List.indexed itemsWithHits do
                    builder.SearchItem(index, item, hits)
            ]

        static member SearchedPrefixesBy(input, prefixesWithHits) =
            Fixture.SearchedItem(Searched.prefix.By(input), prefixesWithHits)

        static member SearchedPrefixesAtStartBy(input, prefixes) =
            Fixture.SearchedPrefixesBy(input, prefixes |> withHitAtStart)

        static member private SearchItemMultiCodes(builder: Searched.CodeHits<_>, itemsWithCodesAndHits) =
            SelectableList.init [
                for index, (item: 'item, codesWithHits) in List.indexed itemsWithCodesAndHits do
                    builder.SearchItemMultiCodes(index, item, codesWithHits = codesWithHits)
            ]

        static member SearchedEmojisBy(input, emojisWithCodesAndHits) =
            Fixture.SearchItemMultiCodes(Searched.emoji.By(input), emojisWithCodesAndHits)

    [<RequireQualifiedAccess>]
    type CompletedStepItem =
        | Prefix of Prefix
        | Emoji of Emoji
        | BreakingChange of BreakingChange
        | SemVerChange of SemVerChange option

        static member ofCompletedStep =
            function
            | CompletedStep.Emoji emoji -> CompletedStepItem.Emoji emoji.Item
            | CompletedStep.Prefix prefix -> CompletedStepItem.Prefix prefix.Item
            | CompletedStep.BreakingChange breakingChange -> CompletedStepItem.BreakingChange breakingChange.Item
            | CompletedStep.SemVerChange semVerChangeOption -> CompletedStepItem.SemVerChange semVerChangeOption

    type Model with
        member this.CompletedStepItems =
            this.CompletedSteps |> List.map CompletedStepItem.ofCompletedStep

    type Field =
        | CurrentStep of Step
        | CurrentInput of string
        | CompletedStepItem of CompletedStepItem
        | IncompleteStepItem of CompletedStepItem
        | CompletedStepItems of CompletedStepItem list
        | SelectablePrefixes of Prefix list
        | SelectableEmojis of Emoji list

    let shouldHave expectedFields (actual: Model) =
        let actualFields = [
            for expectedField in expectedFields do
                match expectedField with
                | CurrentStep _ -> CurrentStep actual.CurrentStep.Step
                | CurrentInput _ -> CurrentInput actual.CurrentStep.Input
                | CompletedStepItem step
                | IncompleteStepItem step ->
                    match actual.CompletedStepItems |> List.contains step with
                    | true -> CompletedStepItem step
                    | false -> IncompleteStepItem step
                | CompletedStepItems _ -> CompletedStepItems actual.CompletedStepItems
                | SelectablePrefixes _ -> SelectablePrefixes actual.AvailablePrefixes
                | SelectableEmojis _ -> SelectableEmojis actual.AvailableEmojis
        ]

        actualFields =! expectedFields

    let private toStringLower input = $"%A{input}".ToLowerInvariant()

    [<RequireQualifiedAccess>]
    type InputMatchingManyEmojis =
        | Arrow
        | Bu
        | Co
        | Cons
        | Heavy
        | Mon
        | Te
        | Z

    let (|InputWithManyMatchingEmojis|) input =
        let result emojisWithCodeAndHits =
            let inputText = toStringLower input
            let expectedEmojis = emojisWithCodeAndHits |> List.map fst

            let expectedStep =
                Step.Emoji(Fixture.SearchedEmojisBy(inputText, emojisWithCodeAndHits))

            inputText, expectedEmojis, expectedStep

        match input with
        | InputMatchingManyEmojis.Arrow ->
            result [
                Emoji.ArrowDown, [ "arrow_down", [ 0 ] ]
                Emoji.ArrowUp, [ "arrow_up", [ 0 ] ]

                Emoji.TwistedRightwardsArrows,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //                  arrow
                    "twisted_rightwards_arrows", [ 19 ]
                    "shuffle_tracks_button", []
                ]
            ]

        | InputMatchingManyEmojis.Bu ->
            result [
                Emoji.Ambulance,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    // bu
                    "ambulance", [ 2 ]
                ]

                Emoji.Bug, [ "bug", [ 0 ] ]
                Emoji.BuildingConstruction, [ "building_construction", [ 0 ] ]

                Emoji.Bulb,
                [
                    "bulb", [ 0 ]
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //     bu
                    "light_bulb", [ 6 ]
                    "idea", []
                ]

                Emoji.BustsInSilhouette, [ "busts_in_silhouette", [ 0 ]; "silhouette_of_two_people", []; "shadow", []; "users", [] ]

                Emoji.Rewind,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //            bu
                    "fast_reverse_button", [ 13 ]
                    "rewind", []
                    "left_pointing_double_triangle", []
                ]

                Emoji.SpeechBalloon,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //    bu
                    "chat_bubble", [ 5 ]
                    "speech_balloon", []
                ]

                Emoji.TwistedRightwardsArrows,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //              bu
                    "shuffle_tracks_button", [ 15 ]
                    "twisted_rightwards_arrows", []
                ]
            ]

        | InputMatchingManyEmojis.Co ->
            result [
                Emoji.Boom, [ "collision", [ 0 ]; "boom", [] ]

                Emoji.BuildingConstruction,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //        co
                    "building_construction", [ 9 ]
                ]

                Emoji.Coffin, [ "coffin", [ 0 ]; "casket", []; "funeral", [] ]
                Emoji.Construction, [ "construction", [ 0 ]; "wip", [] ]
                Emoji.ConstructionWorker, [ "construction_worker", [ 0 ] ]

                Emoji.PassportControl,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //        co
                    "passport_control", [ 9 ]
                ]

                Emoji.SeeNoEvil,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //      co
                    "monkey_covering_eyes", [ 7 ]
                    "see_no_evil", []
                    "mizaru", []
                ]

                Emoji.Stethoscope,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //      co
                    "stethoscope", [ 7 ]
                ]
            ]

        | InputMatchingManyEmojis.Cons ->
            result [
                Emoji.BuildingConstruction,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //        cons
                    "building_construction", [ 9 ]
                ]

                Emoji.Construction, [ "construction", [ 0 ]; "wip", [] ]
                Emoji.ConstructionWorker, [ "construction_worker", [ 0 ] ]
            ]

        | InputMatchingManyEmojis.Heavy ->
            result [ // ↩
                Emoji.HeavyMinusSign, [ "heavy_minus_sign", [ 0 ] ]
                Emoji.HeavyPlusSign, [ "heavy_plus_sign", [ 0 ] ]
            ]

        | InputMatchingManyEmojis.Mon ->
            result [ // ↩
                Emoji.MoneyWithWings, [ "money_with_wings", [ 0 ] ]
                Emoji.MonocleFace, [ "monocle_face", [ 0 ] ]
                Emoji.SeeNoEvil, [ "monkey_covering_eyes", [ 0 ]; "see_no_evil", []; "mizaru", [] ]
            ]

        | InputMatchingManyEmojis.Te ->
            result [
                Emoji.ArtistPalette,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //           te
                    "artist_palette", [ 12 ]
                ]

                Emoji.BustsInSilhouette,
                [
                    //.........1.........2..........3...
                    //1234567890123456789012234567890123
                    //                te
                    "busts_in_silhouette", [ 17 ]
                    //       te
                    "silhouette_of_two_people", [ 8 ]
                    "shadow", []
                    "users", []
                ]

                Emoji.CheckMark,
                [
                    //123456789
                    //  te
                    "white_check_mark", [ 3 ]
                    "check_mark", []
                    "green_tick", []
                ]

                Emoji.Mute, [ "mute", [ 2 ] ]

                Emoji.Sparkles,
                [
                    //123456789
                    //   te
                    "glitter", [ 4 ]
                    "sparkles", []
                    "shiny", []
                ]

                Emoji.Stethoscope,
                [
                    //123456789
                    //te
                    "stethoscope", [ 1 ]
                ]

                Emoji.Technologist, [ "technologist", [ 0 ] ]
                Emoji.TestTube, [ "test_tube", [ 0 ] ]

                Emoji.TwistedRightwardsArrows,
                [
                    //123456789
                    //   te
                    "twisted_rightwards_arrows", [ 4 ]
                    "shuffle_tracks_button", []
                ]

                // ..    ↓
                Emoji.Wastebasket,
                [
                    //123456789
                    //  te
                    "wastebasket", [ 3 ]
                    "wastepaper_basket", [ 3 ]
                    "garbage_can", []
                    "rubbish_bin", []
                    "trash_can", []
                ]
            ]

        | InputMatchingManyEmojis.Z ->
            result [
                Emoji.Dizzy,
                [
                    //1234
                    // zz
                    "dizzy", [ 2; 3 ]
                ]

                Emoji.SeeNoEvil,
                [
                    //1234
                    // z
                    "mizaru", [ 2 ]
                    "see_no_evil", []
                    "monkey_covering_eyes", []
                ]

                Emoji.Zap, [ "zap", [ 0 ]; "high_voltage", []; "lightning_bolt", []; "thunderbolt", [] ]
            ]

    [<RequireQualifiedAccess>]
    type InputMatchingManyPrefixes =
        | Blank
        | F
        | Re

    let (|InputWithManyMatchingPrefixes|) input =
        let fixture = Fixture.Initial

        let searchable prefixes = "", fixture.SearchablePrefixes prefixes

        let searchedBy inputText prefixes =
            inputText, Fixture.SearchedPrefixesAtStartBy(inputText, prefixes)

        let inputText, selectableList =
            match input with
            | InputMatchingManyPrefixes.Blank -> Prefix.All |> searchable
            | InputMatchingManyPrefixes.F -> [ Prefix.Feat; Prefix.Fix ] |> searchedBy "f"
            | InputMatchingManyPrefixes.Re -> [ Prefix.Refactor; Prefix.Revert ] |> searchedBy "re"

        fixture.Model, inputText, selectableList

    [<RequireQualifiedAccess>]
    type MinInputToMatchExactlyOneEmojiAndOnePrefix =
        | Adh
        | Alie
        | Bee
        | Book
        | Bug
        | Bui
        | Bulb
        | HeavyP
        | Mag
        | Mone
        | Pack
        | Pag
        | Pas
        | Pu
        | Rec
        | Roc
        | Rot
        | Sa
        | See_
        | Spar
        | Tes
        | Thre
        | Whe
        | Whi
        | Zap

    let (|MinInputWithMatchingEmojiWithSinglePrefix|) =
        function
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Adh -> "adh", Emoji.AdhesiveBandage, Prefix.Fix
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Alie -> "alie", Emoji.Alien, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Bee -> "bee", Emoji.Beers, Prefix.Wip
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Book -> "book", Emoji.Bookmark, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Bug -> "bug", Emoji.Bug, Prefix.Fix
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Bui -> "bui", Emoji.BuildingConstruction, Prefix.Refactor
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Bulb -> "bulb", Emoji.Bulb, Prefix.Docs
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.HeavyP -> "heavy_p", Emoji.HeavyPlusSign, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Mag -> "mag", Emoji.Mag, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Mone -> "mone", Emoji.MoneyWithWings, Prefix.Docs
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pack -> "pack", Emoji.Package, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pag -> "pag", Emoji.PageFacingUp, Prefix.Docs
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pas -> "pas", Emoji.PassportControl, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pu -> "pu", Emoji.Pushpin, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Rec -> "rec", Emoji.Recycle, Prefix.Refactor
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Roc -> "roc", Emoji.Rocket, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Rot -> "rot", Emoji.RotatingLight, Prefix.Fix
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Sa -> "sa", Emoji.SafetyVest, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.See_ -> "see_", Emoji.SeeNoEvil, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Spar -> "spar", Emoji.Sparkles, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Tes -> "tes", Emoji.TestTube, Prefix.Test
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Thre -> "thre", Emoji.Thread, Prefix.Perf
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Whe -> "whe", Emoji.Wheelchair, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Whi -> "whi", Emoji.CheckMark, Prefix.Test
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Zap -> "zap", Emoji.Zap, Prefix.Perf

    [<RequireQualifiedAccess>]
    type MinInputToMatchExactlyOneEmojiAndManyPrefixes =
        | Boom
        | Bro
        | Cam
        | Diz
        | Mono
        | Rew
        | Triangular

    let (|MinInputWithMatchingEmojiWithManyPrefixes|) =
        function
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Boom -> "boom", Emoji.Boom, [ Prefix.Feat; Prefix.Fix ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Bro -> "bro", Emoji.Broom, [ Prefix.Refactor; Prefix.Chore ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Cam -> "cam", Emoji.CameraFlash, [ Prefix.Test; Prefix.Chore; Prefix.Docs ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Diz -> "diz", Emoji.Dizzy, [ Prefix.Feat; Prefix.Fix ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Mono -> "mono", Emoji.MonocleFace, [ Prefix.Chore; Prefix.Wip ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Rew -> "rew", Emoji.Rewind, [ Prefix.Chore; Prefix.Revert ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Triangular -> "triangular", Emoji.TriangularFlagOnPost, [ Prefix.Feat; Prefix.Chore ]

    [<RequireQualifiedAccess>]
    type MinInputToMatchExactlyOnePrefixAndManyEmojis =
        | C
        | D
        | Fe
        | Fi
        | P
        | Ref
        // Not Rev because it has a single emoji: Emoji.Rewind
        | T
        | W

    let (|MinInputWithMatchingPrefixAndEmojis|) input =
        let inputText = $"%A{input}".ToLowerInvariant()

        let result prefix emojis = // ↩
            inputText, prefix, emojis

        match input with
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.C ->
            result Prefix.Chore [
                Emoji.Alembic
                Emoji.Alien
                Emoji.ArrowDown
                Emoji.ArrowUp
                Emoji.Bento
                Emoji.Bookmark
                Emoji.Bricks
                Emoji.Broom
                Emoji.CameraFlash
                Emoji.CardFileBox
                Emoji.ClosedLockWithKey
                Emoji.ConstructionWorker
                Emoji.GreenHeart
                Emoji.Hammer
                Emoji.HeavyMinusSign
                Emoji.HeavyPlusSign
                Emoji.LoudSound
                Emoji.Mag
                Emoji.MonocleFace
                Emoji.Mute
                Emoji.Package
                Emoji.Pencil
                Emoji.Pushpin
                Emoji.Rewind
                Emoji.Rocket
                Emoji.Seedling
                Emoji.SeeNoEvil
                Emoji.SpeechBalloon
                Emoji.Stethoscope
                Emoji.TriangularFlagOnPost
                Emoji.TwistedRightwardsArrows
                Emoji.Wrench
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.D ->
            result Prefix.Docs [
                Emoji.Bulb
                Emoji.BustsInSilhouette
                Emoji.CameraFlash
                Emoji.Memo
                Emoji.MoneyWithWings
                Emoji.PageFacingUp
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.Fe ->
            result Prefix.Feat [
                Emoji.Airplane
                Emoji.Alembic
                Emoji.Boom
                Emoji.Bricks
                Emoji.CardFileBox
                Emoji.ChartWithUpwardsTrend
                Emoji.ChildrenCrossing
                Emoji.Construction
                Emoji.Dizzy
                Emoji.Egg
                Emoji.GlobeWithMeridians
                Emoji.GoalNet
                Emoji.Iphone
                Emoji.Label
                Emoji.Lipstick
                Emoji.Necktie
                Emoji.PassportControl
                Emoji.Poop
                Emoji.SafetyVest
                Emoji.Seedling
                Emoji.Sparkles
                Emoji.TriangularFlagOnPost
                Emoji.Wheelchair
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.Fi ->
            result Prefix.Fix [
                Emoji.AdhesiveBandage
                Emoji.Ambulance
                Emoji.Boom
                Emoji.Bug
                Emoji.ChartWithUpwardsTrend
                Emoji.Dizzy
                Emoji.Iphone
                Emoji.Lipstick
                Emoji.Lock
                Emoji.RotatingLight
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.P ->
            result Prefix.Perf [
                Emoji.Thread // ↩
                Emoji.Zap
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.Ref ->
            result Prefix.Refactor [
                Emoji.ArtistPalette
                Emoji.Broom
                Emoji.BuildingConstruction
                Emoji.Coffin
                Emoji.Fire
                Emoji.Recycle
                Emoji.Technologist
                Emoji.Truck
                Emoji.Wastebasket
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.T ->
            result Prefix.Test [
                Emoji.CameraFlash
                Emoji.CheckMark
                Emoji.ClownFace
                Emoji.Seedling
                Emoji.TestTube
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.W ->
            result Prefix.Wip [
                Emoji.Alembic
                Emoji.Beers
                Emoji.Construction
                Emoji.MonocleFace
                Emoji.Poop
                Emoji.Tada
                Emoji.Wastebasket
            ]

    let (|MinInputMatchingFeatOrFixWithEmojis|) isFeat =
        let (MinInputWithMatchingPrefixAndEmojis(input, expectedPrefix, expectedEmojis)) =
            match isFeat with
            | true -> MinInputToMatchExactlyOnePrefixAndManyEmojis.Fe
            | false -> MinInputToMatchExactlyOnePrefixAndManyEmojis.Fi

        input, expectedPrefix, expectedEmojis

    [<RequireQualifiedAccess>]
    type PrefixWithoutBreakingChange =
        | Refactor
        | Test
        | Chore
        | Docs
        | Perf
        | Wip

    let (|MinInputWithMatchingEmojiWithManyPrefixesAndNoBreakingChange|) prefixWithoutBreakingChange =
        let (MinInputWithMatchingPrefixAndEmojis(input, expectedPrefix, expectedEmojis)) =
            match prefixWithoutBreakingChange with
            | PrefixWithoutBreakingChange.Chore -> MinInputToMatchExactlyOnePrefixAndManyEmojis.C
            | PrefixWithoutBreakingChange.Docs -> MinInputToMatchExactlyOnePrefixAndManyEmojis.D
            | PrefixWithoutBreakingChange.Perf -> MinInputToMatchExactlyOnePrefixAndManyEmojis.P
            | PrefixWithoutBreakingChange.Refactor -> MinInputToMatchExactlyOnePrefixAndManyEmojis.Ref
            | PrefixWithoutBreakingChange.Test -> MinInputToMatchExactlyOnePrefixAndManyEmojis.T
            | PrefixWithoutBreakingChange.Wip -> MinInputToMatchExactlyOnePrefixAndManyEmojis.W

        input, expectedPrefix, expectedEmojis

module ``0_ init`` =
    [<Fact>]
    let ``start at the prefix step with all possible prefixes and emojis`` () =
        let initial = Fixture.Initial.Model

        (initWith initial.SearchMode) =! initial

module ``1_ select prefix first`` =
    [<Property>]
    let ``filter prefixes to match the given input`` (InputWithManyMatchingPrefixes(model, input, expectedPrefixes)) =
        let actual =
            model // ↩
            |> update (Msg.InputChanged input)

        actual
        |> shouldHave [
            CurrentStep(Step.Prefix expectedPrefixes) // ↩
            CurrentInput input
        ]

    [<Property>]
    let ``select the first prefix matching the given input when pressing [Enter]`` (InputWithManyMatchingPrefixes(model, input, expectedPrefixes)) =
        let expectedPrefix = expectedPrefixes.Items.Head.Item

        let actual =
            model // ↩
            |> update (Msg.InputChanged input)
            |> update Msg.AcceptSelection

        actual |> shouldHave [ CompletedStepItems [ CompletedStepItem.Prefix expectedPrefix ] ]

    [<Property>]
    let ``select the exact matching prefix`` (MinInputWithMatchingPrefixAndEmojis(exactPrefixInput, expectedPrefix, _)) =
        let actual =
            Fixture.Initial.Model // ↩
            |> update (Msg.InputChanged exactPrefixInput)

        actual |> shouldHave [ CompletedStepItem(CompletedStepItem.Prefix expectedPrefix) ]

module ``2_ select emoji after prefix`` =
    let fixture = Fixture.Initial
    let initial = fixture.Model

    [<Property>]
    let ``limit the selectable emojis according to the selected prefix`` (MinInputWithMatchingPrefixAndEmojis(exactPrefixInput, _, expectedEmojis)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)

        actual
        |> shouldHave [
            CurrentStep(Step.Emoji(fixture.SearchableEmojis expectedEmojis)) // ↩
            CurrentInput ""
        ]

    [<Property>]
    let ``select the first emoji matching the selected prefix when pressing [Enter]`` (MinInputWithMatchingPrefixAndEmojis(exactPrefixInput, expectedPrefix, expectedEmojis)) =
        let expectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.AcceptSelection

        actual
        |> shouldHave [
            CompletedStepItem(CompletedStepItem.Prefix expectedPrefix) // ↩
            CompletedStepItem(CompletedStepItem.Emoji expectedEmoji)
        ]

    [<Fact>]
    let ``select both Revert prefix and Rewind emoji when inputting 'rev'`` () =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged "rev")

        actual
        |> shouldHave [
            CompletedStepItem(CompletedStepItem.Prefix Prefix.Revert) // ↩
            CompletedStepItem(CompletedStepItem.Emoji Emoji.Rewind)
        ]

module ``3_ select emoji first`` =
    [<Fact>]
    let ``switch to emoji selection`` () =
        let fixture = Fixture.Initial

        let actual =
            fixture.Model // ↩
            |> update Msg.ToggleFirstStepToEmoji

        actual
        |> shouldHave [
            CurrentStep(Step.Emoji(fixture.SearchableEmojis Emoji.All)) // ↩
            CurrentInput ""
        ]

    let initial = Fixture.InitialForEmojis.Model

    [<Property>]
    let ``filter emojis to match the given input`` (InputWithManyMatchingEmojis(input, _, expectedStep)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged input)

        actual
        |> shouldHave [
            CurrentStep expectedStep // ↩
            CurrentInput input
        ]

    [<Property>]
    let ``select the first emoji matching the given input when pressing [Enter]`` (InputWithManyMatchingEmojis(input, expectedEmojis, _)) =
        let expectedSelectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged input)
            |> update Msg.AcceptSelection

        actual |> shouldHave [ CompletedStepItem(CompletedStepItem.Emoji expectedSelectedEmoji) ]

    [<Property>]
    let ``select the emoji matching exactly the given input`` (MinInputWithMatchingEmojiWithManyPrefixes(exactEmojiInput, expectedSelectedEmoji, _)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactEmojiInput)

        actual |> shouldHave [ CompletedStepItem(CompletedStepItem.Emoji expectedSelectedEmoji) ]

module ``4_ select prefix after emoji`` =
    let initial = Fixture.InitialForEmojis.Model

    [<Property>]
    let ``limit the selectable prefixes according to the selected emoji`` (MinInputWithMatchingEmojiWithManyPrefixes(exactEmojiInput, _, expectedPrefixes)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactEmojiInput)

        actual
        |> shouldHave [
            CurrentStep(Step.Prefix(SelectableList.Prefixes.searchable expectedPrefixes initial.SearchMode.SegmentsConfiguration)) // ↩
            CurrentInput ""
        ]

    [<Property>]
    let ``select the first prefix when pressing [Enter]`` (MinInputWithMatchingEmojiWithManyPrefixes(exactEmojiInput, expectedSelectedEmoji, expectedPrefixes)) =
        let expectedPrefix = expectedPrefixes.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactEmojiInput)
            |> update Msg.AcceptSelection

        actual
        |> shouldHave [
            CompletedStepItem(CompletedStepItem.Prefix expectedPrefix) // ↩
            CompletedStepItem(CompletedStepItem.Emoji expectedSelectedEmoji)
        ]

    [<Property>]
    let ``select directly the single possible prefix`` (MinInputWithMatchingEmojiWithSinglePrefix(exactEmojiInput, expectedSelectedEmoji, expectedPrefix)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactEmojiInput)

        actual
        |> shouldHave [
            CompletedStepItem(CompletedStepItem.Prefix expectedPrefix) // ↩
            CompletedStepItem(CompletedStepItem.Emoji expectedSelectedEmoji)
        ]

module ``5_ select breaking change`` =
    let initial = Fixture.Initial.Model

    [<Theory>]
    [<InlineData("fe")>]
    [<InlineData("fi")>]
    let ``force breaking change when selecting Boom emoji after feat or fix prefixes`` input =
        let expectedPrefix = if input = "fe" then Prefix.Feat else Prefix.Fix

        let actual =
            initial // ↩
            |> update (Msg.InputChanged input)
            |> update (Msg.InputChanged "boo")

        actual
        |> shouldHave [
            CompletedStepItem(CompletedStepItem.Prefix expectedPrefix) // ↩
            CompletedStepItem(CompletedStepItem.Emoji Emoji.Boom)
            CompletedStepItem(CompletedStepItem.BreakingChange BreakingChange.Yes)
        ]

    [<Property>]
    let ``select the breaking change when pressing [y] given a feat or a fix`` (MinInputMatchingFeatOrFixWithEmojis(exactPrefixInput, expectedPrefix, expectedEmojis)) =
        let expectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.AcceptSelection // Select first emoji
            |> update (Msg.InputChanged "y") // Confirm breaking change

        actual
        |> shouldHave [
            CompletedStepItem(CompletedStepItem.Prefix expectedPrefix) // ↩
            CompletedStepItem(CompletedStepItem.Emoji expectedEmoji)
            CompletedStepItem(CompletedStepItem.BreakingChange BreakingChange.Yes)
        ]

    [<Property>]
    let ``confirm no breaking change when pressing [Enter] given a feat or a fix`` (MinInputMatchingFeatOrFixWithEmojis(exactPrefixInput, expectedPrefix, expectedEmojis)) =
        let expectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.AcceptSelection // Select first emoji
            |> update Msg.AcceptSelection // Confirm no breaking change

        actual
        |> shouldHave [
            CompletedStepItem(CompletedStepItem.Prefix expectedPrefix) // ↩
            CompletedStepItem(CompletedStepItem.Emoji expectedEmoji)
            CompletedStepItem(CompletedStepItem.BreakingChange BreakingChange.No)
        ]

    [<Property>]
    let ``prevent selecting the breaking change given neither a feat nor a fix`` (MinInputWithMatchingEmojiWithManyPrefixesAndNoBreakingChange(exactPrefixInput, expectedPrefix, expectedEmojis)) =
        let expectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.AcceptSelection

        actual
        |> shouldHave [
            CompletedStepItem(CompletedStepItem.Prefix expectedPrefix) // ↩
            CompletedStepItem(CompletedStepItem.Emoji expectedEmoji)
            CompletedStepItem(CompletedStepItem.BreakingChange BreakingChange.No)
        ]

module ``6_ determine semantic version change`` =
    let fixture = Fixture.Initial
    let initial = fixture.Model

    [<Property>]
    let ``indicate major given a feat or a fix with breaking change`` (MinInputMatchingFeatOrFixWithEmojis(exactPrefixInput, _, _)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.AcceptSelection // Select first emoji
            |> update (Msg.InputChanged "y") // Confirm breaking change

        actual |> shouldHave [ CompletedStepItem(CompletedStepItem.SemVerChange(Some SemVerChange.Major)) ]

    [<Fact>]
    let ``indicate minor given a feat with no breaking change`` () =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged "fe")
            |> update Msg.AcceptSelection // Select first emoji
            |> update Msg.AcceptSelection // Confirm no breaking change

        actual |> shouldHave [ CompletedStepItem(CompletedStepItem.SemVerChange(Some SemVerChange.Minor)) ]

    [<Fact>]
    let ``indicate patch given a fix with no breaking change`` () =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged "fi")
            |> update Msg.AcceptSelection // Select first emoji
            |> update Msg.AcceptSelection // Confirm no breaking change

        actual |> shouldHave [ CompletedStepItem(CompletedStepItem.SemVerChange(Some SemVerChange.Patch)) ]

    [<Property>]
    let ``indicate none given other prefix`` (MinInputWithMatchingEmojiWithManyPrefixesAndNoBreakingChange(exactPrefixInput, _, _)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.AcceptSelection // Select first emoji - No possible breaking change detected

        actual |> shouldHave [ CompletedStepItem(CompletedStepItem.SemVerChange None) ]