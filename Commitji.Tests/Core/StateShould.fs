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
                        Map [ SegmentId.Code, SegmentState.Searchable operation ]
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

            member _.SearchItem(index, item, hits) = {
                Item = item
                Index = index
                Segments = [
                    {
                        Id = SegmentId.Code
                        Text = getCode item
                        State = SegmentState.Searched(hits, length)
                    }
                ]
            }

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

        static member SearchedEmojisBy(input, emojisWithHits) =
            Fixture.SearchedItem(Searched.emoji.By(input), emojisWithHits)

        static member SearchedEmojisAtStartBy(input, emojis) =
            Fixture.SearchedItem(Searched.emoji.By(input), emojis |> withHitAtStart)

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
        let result emojisWithHits =
            let inputText = toStringLower input
            let expectedEmojis = emojisWithHits |> List.map fst
            let expectedStep = Step.Emoji(Fixture.SearchedEmojisBy(inputText, emojisWithHits))
            inputText, expectedEmojis, expectedStep

        match input with
        | InputMatchingManyEmojis.Arrow ->
            result [
                // .. 0         1         2
                // .. 01234567890123456789012345

                // .. ↓
                Emoji.ArrowDown, [ 0 ]
                Emoji.ArrowUp, [ 0 ]

                // ..                    ↓
                // .. twisted_rightwards_arrows
                Emoji.TwistedRightwardsArrows, [ 19 ]
            ]

        | InputMatchingManyEmojis.Bu ->
            result [
                // .. 0         1         2
                // .. 01234567890123456789012345

                // ..   ↓
                Emoji.Ambulance, [ 2 ]

                // .. ↓
                Emoji.Bug, [ 0 ]
                Emoji.BuildingConstruction, [ 0 ]
                Emoji.Bulb, [ 0 ]
                Emoji.BustsInSilhouette, [ 0 ]
            ]
        | InputMatchingManyEmojis.Co ->
            result [
                // .. 0         1         2
                // .. 01234567890123456789012345

                // ..          ↓
                // .. building_construction
                Emoji.BuildingConstruction, [ 9 ]

                // .. ↓
                Emoji.Coffin, [ 0 ]
                Emoji.Construction, [ 0 ]
                Emoji.ConstructionWorker, [ 0 ]

                // ..          ↓
                // .. passport_control
                Emoji.PassportControl, [ 9 ]

                // ..        ↓
                Emoji.Stethoscope, [ 7 ]
            ]

        | InputMatchingManyEmojis.Cons ->
            result [
                // .. 0         1         2
                // .. 01234567890123456789012345

                // ..          ↓
                // .. building_construction
                Emoji.BuildingConstruction, [ 9 ]

                // .. ↓
                Emoji.Construction, [ 0 ]
                Emoji.ConstructionWorker, [ 0 ]
            ]

        | InputMatchingManyEmojis.Heavy ->
            result [
                // .. 0         1         2
                // .. 01234567890123456789012345
                // .. ↓
                Emoji.HeavyMinusSign, [ 0 ]
                Emoji.HeavyPlusSign, [ 0 ]
            ]

        | InputMatchingManyEmojis.Mon ->
            result [
                // .. 0         1         2
                // .. 01234567890123456789012345
                // .. ↓
                Emoji.MoneyWithWings, [ 0 ]
                Emoji.MonocleFace, [ 0 ]
            ]

        | InputMatchingManyEmojis.Te ->
            result [
                // .. 0         1         2
                // .. 01234567890123456789012345

                // ..                  ↓
                // .. busts_in_silhouette
                Emoji.BustsInSilhouette, [ 17 ]

                // ..   ↓
                Emoji.Mute, [ 2 ]

                // ..  ↓
                Emoji.Stethoscope, [ 1 ]

                // .. ↓
                Emoji.Technologist, [ 0 ]
                Emoji.TestTube, [ 0 ]

                // ..     ↓
                Emoji.TwistedRightwardsArrows, [ 4 ]

                // ..    ↓
                Emoji.Wastebasket, [ 3 ]
                Emoji.WhiteCheckMark, [ 3 ]
            ]

        | InputMatchingManyEmojis.Z ->
            result [
                // .. 0         1         2
                // .. 01234567890123456789012345

                // ..   ↓↓  👈 double-hit - no-conflation to avoid over-engineering low-impact optimisation
                Emoji.Dizzy, [ 2; 3 ]

                // .. ↓
                Emoji.Zap, [ 0 ]
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
        | Pac
        | Pag
        | Pas
        | Pu
        | Rec
        | Roc
        | Rot
        | Sa
        | See_
        | Spa
        | Tes
        | Thr
        | Whe
        | Whi
        | Za

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
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pac -> "pac", Emoji.Package, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pag -> "pag", Emoji.PageFacingUp, Prefix.Docs
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pas -> "pas", Emoji.PassportControl, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pu -> "pu", Emoji.Pushpin, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Rec -> "rec", Emoji.Recycle, Prefix.Refactor
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Roc -> "roc", Emoji.Rocket, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Rot -> "rot", Emoji.RotatingLight, Prefix.Fix
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Sa -> "sa", Emoji.SafetyVest, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.See_ -> "see_", Emoji.SeeNoEvil, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Spa -> "spa", Emoji.Sparkles, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Tes -> "tes", Emoji.TestTube, Prefix.Test
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Thr -> "thr", Emoji.Thread, Prefix.Perf
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Whe -> "whe", Emoji.Wheelchair, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Whi -> "whi", Emoji.WhiteCheckMark, Prefix.Test
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Za -> "za", Emoji.Zap, Prefix.Perf

    [<RequireQualifiedAccess>]
    type MinInputToMatchExactlyOneEmojiAndManyPrefixes =
        | Ale
        | Boom
        | Cam
        | Diz
        | Mono
        | Rew
        | Tri

    let (|MinInputWithMatchingEmojiWithManyPrefixes|) =
        function
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Ale -> "ale", Emoji.Alembic, [ Prefix.Feat; Prefix.Chore; Prefix.Wip ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Boom -> "boom", Emoji.Boom, [ Prefix.Feat; Prefix.Fix ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Cam -> "cam", Emoji.CameraFlash, [ Prefix.Test; Prefix.Chore; Prefix.Docs ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Diz -> "diz", Emoji.Dizzy, [ Prefix.Feat; Prefix.Fix ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Mono -> "mono", Emoji.MonocleFace, [ Prefix.Chore; Prefix.Wip ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Rew -> "rew", Emoji.Rewind, [ Prefix.Chore; Prefix.Revert ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Tri -> "tri", Emoji.TriangularFlagOnPost, [ Prefix.Feat; Prefix.Chore ]

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
            result Chore [
                Emoji.Alembic
                Emoji.Alien
                Emoji.ArrowDown
                Emoji.ArrowUp
                Emoji.Bento
                Emoji.Bookmark
                Emoji.Bricks
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
                Emoji.Pencil2
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
            result Docs [
                Emoji.Bulb
                Emoji.BustsInSilhouette
                Emoji.CameraFlash
                Emoji.Memo
                Emoji.MoneyWithWings
                Emoji.PageFacingUp
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.Fe ->
            result Feat [
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
            result Fix [
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
            result Perf [
                Emoji.Thread // ↩
                Emoji.Zap
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.Ref ->
            result Refactor [
                Emoji.Art
                Emoji.BuildingConstruction
                Emoji.Coffin
                Emoji.Fire
                Emoji.Recycle
                Emoji.Technologist
                Emoji.Truck
                Emoji.Wastebasket
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.T ->
            result Test [
                Emoji.CameraFlash
                Emoji.ClownFace
                Emoji.Seedling
                Emoji.TestTube
                Emoji.WhiteCheckMark
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.W ->
            result Wip [
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
    let ``indicate minor given a fix with no breaking change`` () =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged "fi")
            |> update Msg.AcceptSelection // Select first emoji
            |> update Msg.AcceptSelection // Confirm no breaking change

        actual |> shouldHave [ CompletedStepItem(CompletedStepItem.SemVerChange(Some SemVerChange.Minor)) ]

    [<Fact>]
    let ``indicate patch given a feat with no breaking change`` () =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged "fe")
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

// TODO
// module ``7_ confirm selections`` =
//     let private (|ModelReadyForConfirmation|) (prefix, emoji, breakingChange, semVerChange) =
//         {
//             Fixture.Initial.Model with
//                 CurrentStep = { Step = Step.Confirmation; Input = "" }
//                 CompletedSteps = [
//                     // The combination (prefix, emoji, breakingChange, semVerChange) generated by FsCheck is random,
//                     // so it probably doesn't match the real relations, but it should not be important for this test
//                     CompletedStep.Prefix prefix // ↩
//                     CompletedStep.Emoji emoji
//                     CompletedStep.BreakingChange breakingChange
//                     CompletedStep.SemVerChange semVerChange
//                 ]
//         },
//         (prefix, emoji, breakingChange)
//
//     [<Property>]
//     let ``any input is considered invalid at this step`` (NonEmptyString invalidInput) (ModelReadyForConfirmation(model, _)) =
//         let actual = model |> update (Msg.InputChanged invalidInput)
//         actual |> shouldHave [ CurrentStep(model.CurrentStep.Step |> Step.setInvalidInput invalidInput) ]
//
//     [<Property>]
//     let ``confirm all selections by pressing [Enter]`` (ModelReadyForConfirmation(model, (prefix, emoji, breakingChange))) =
//         let actual = model |> update Msg.Enter
//         ()
