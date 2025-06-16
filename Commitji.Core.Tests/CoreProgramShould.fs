module CoreProgramShould

open Commitji.Core.Model
open Commitji.Core.State
open Commitji.Core.Types
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open global.Xunit

[<AutoOpen>]
module Helpers =
    let initial = {
        CurrentStep = {
            Step = Step.Prefix Prefix.All
            Input = ""
            Confirmed = false
        }
        CompletedSteps = []
        SelectablePrefixes = Prefix.All
        SelectableEmojis = Emoji.All
        PreviousFullCompletion = None
    }

    type Field =
        | CurrentStep of Step
        | CurrentInput of string
        | CompleteStep of CompletedStep
        | IncompleteStep of CompletedStep
        | CompletedSteps of CompletedStep list
        | SelectablePrefixes of Prefix list
        | SelectableEmojis of Emoji list
        | PreviousFullCompletion of (Prefix * Emoji * BreakingChange) option

    let shouldHave expectedFields (actual: Model) =
        let actualFields = [
            for expectedField in expectedFields do
                match expectedField with
                | CurrentStep _ -> CurrentStep actual.CurrentStep.Step
                | CurrentInput _ -> CurrentInput actual.CurrentStep.Input
                | CompleteStep step
                | IncompleteStep step ->
                    match actual.CompletedSteps |> List.contains step with
                    | true -> CompleteStep step
                    | false -> IncompleteStep step
                | CompletedSteps _ -> CompletedSteps actual.CompletedSteps
                | SelectablePrefixes _ -> SelectablePrefixes actual.SelectablePrefixes
                | SelectableEmojis _ -> SelectableEmojis actual.SelectableEmojis
                | PreviousFullCompletion _ -> PreviousFullCompletion actual.PreviousFullCompletion
        ]

        actualFields =! expectedFields

    [<RequireQualifiedAccess>]
    type InputMatchingManyEmojis =
        | Arrow
        | Bu
        | Co
        | Cons
        | Construction
        | G
        | Heavy
        | Mon
        | Te
        | W

    let (|InputWithManyMatchingEmojis|) =
        function
        | InputMatchingManyEmojis.Arrow -> "arrow", [ Emoji.ArrowDown; Emoji.ArrowUp ]
        | InputMatchingManyEmojis.Bu -> "bu", [ Emoji.Bug; Emoji.BuildingConstruction; Emoji.Bulb; Emoji.BustsInSilhouette ]
        | InputMatchingManyEmojis.Co -> "co", [ Emoji.Coffin; Emoji.Construction; Emoji.ConstructionWorker ]
        | InputMatchingManyEmojis.Cons -> "cons", [ Emoji.Construction; Emoji.ConstructionWorker ]
        | InputMatchingManyEmojis.Construction -> "construction", [ Emoji.Construction; Emoji.ConstructionWorker ]
        | InputMatchingManyEmojis.G -> "g", [ Emoji.GlobeWithMeridians; Emoji.GoalNet; Emoji.GreenHeart ]
        | InputMatchingManyEmojis.Heavy -> "heavy", [ Emoji.HeavyMinusSign; Emoji.HeavyPlusSign ]
        | InputMatchingManyEmojis.Mon -> "mon", [ Emoji.MoneyWithWings; Emoji.MonocleFace ]
        | InputMatchingManyEmojis.Te -> "te", [ Emoji.Technologist; Emoji.TestTube ]
        | InputMatchingManyEmojis.W -> "w", [ Emoji.Wastebasket; Emoji.Wheelchair; Emoji.WhiteCheckMark; Emoji.Wrench ]

    [<RequireQualifiedAccess>]
    type InputMatchingManyPrefixes =
        | Blank
        | F
        | R
        | Re

    let (|InputWithManyMatchingPrefixes|) =
        function
        | InputMatchingManyPrefixes.Blank -> "", Prefix.All
        | InputMatchingManyPrefixes.F -> "f", [ Prefix.Feat; Prefix.Fix ]
        | InputMatchingManyPrefixes.R -> "r", [ Prefix.Refactor; Prefix.Revert ]
        | InputMatchingManyPrefixes.Re -> "re", [ Prefix.Refactor; Prefix.Revert ]

    [<RequireQualifiedAccess>]
    type MinInputToMatchExactlyOneEmojiAndOnePrefix =
        | Ad
        | Ai
        | Ali
        | Bee
        | Book
        | Bug
        | Bui
        | Bul
        | E
        | HeavyP
        | Loc
        | Ma
        | Me
        | Mone
        | N
        | Pac
        | Pag
        | Pas
        | Pe
        | Pu
        | Rec
        | Roc
        | Rot
        | Sa
        | See_
        | Spa
        | Tes
        | Th
        | Tru
        | Whe
        | Whi
        | Z

    let (|MinInputWithMatchingEmojiWithSinglePrefix|) =
        function
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Ad -> "ad", Emoji.AdhesiveBandage, Prefix.Fix
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Ai -> "ai", Emoji.Airplane, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Ali -> "ali", Emoji.Alien, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Bee -> "bee", Emoji.Beers, Prefix.Wip
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Book -> "book", Emoji.Bookmark, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Bug -> "bug", Emoji.Bug, Prefix.Fix
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Bui -> "bui", Emoji.BuildingConstruction, Prefix.Refactor
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Bul -> "bul", Emoji.Bulb, Prefix.Docs
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.E -> "e", Emoji.Egg, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.HeavyP -> "heavy_p", Emoji.HeavyPlusSign, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Loc -> "loc", Emoji.Lock, Prefix.Fix
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Ma -> "ma", Emoji.Mag, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Me -> "me", Emoji.Memo, Prefix.Docs
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Mone -> "mone", Emoji.MoneyWithWings, Prefix.Docs
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.N -> "n", Emoji.Necktie, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pac -> "pac", Emoji.Package, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pag -> "pag", Emoji.PageFacingUp, Prefix.Docs
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pas -> "pas", Emoji.PassportControl, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pe -> "pe", Emoji.Pencil2, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Pu -> "pu", Emoji.Pushpin, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Rec -> "rec", Emoji.Recycle, Prefix.Refactor
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Roc -> "roc", Emoji.Rocket, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Rot -> "rot", Emoji.RotatingLight, Prefix.Fix
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Sa -> "sa", Emoji.SafetyVest, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.See_ -> "see_", Emoji.SeeNoEvil, Prefix.Chore
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Spa -> "spa", Emoji.Sparkles, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Tes -> "tes", Emoji.TestTube, Prefix.Test
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Th -> "th", Emoji.Thread, Prefix.Perf
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Tru -> "tru", Emoji.Truck, Prefix.Refactor
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Whe -> "whe", Emoji.Wheelchair, Prefix.Feat
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Whi -> "whi", Emoji.WhiteCheckMark, Prefix.Test
        | MinInputToMatchExactlyOneEmojiAndOnePrefix.Z -> "z", Emoji.Zap, Prefix.Perf

    [<RequireQualifiedAccess>]
    type MinInputToMatchExactlyOneEmojiAndManyPrefixes =
        | Ale
        | Boom
        | Br
        | Cam
        | D
        | I
        | Li
        | Mono
        | Po
        | Rew
        | Tri
        | Wa

    let (|MinInputWithMatchingEmojiWithManyPrefixes|) =
        function
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Ale -> "ale", Emoji.Alembic, [ Prefix.Feat; Prefix.Chore; Prefix.Wip ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Boom -> "boom", Emoji.Boom, [ Prefix.Feat; Prefix.Fix ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Br -> "br", Emoji.Bricks, [ Prefix.Feat; Prefix.Chore ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Cam -> "cam", Emoji.CameraFlash, [ Prefix.Test; Prefix.Chore; Prefix.Docs ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.D -> "d", Emoji.Dizzy, [ Prefix.Feat; Prefix.Fix ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.I -> "i", Emoji.Iphone, [ Prefix.Feat; Prefix.Fix ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Li -> "li", Emoji.Lipstick, [ Prefix.Feat; Prefix.Fix ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Mono -> "mono", Emoji.MonocleFace, [ Prefix.Chore; Prefix.Wip ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Po -> "po", Emoji.Poop, [ Prefix.Feat; Prefix.Wip ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Rew -> "rew", Emoji.Rewind, [ Prefix.Chore; Prefix.Revert ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Tri -> "tri", Emoji.TriangularFlagOnPost, [ Prefix.Feat; Prefix.Chore ]
        | MinInputToMatchExactlyOneEmojiAndManyPrefixes.Wa -> "wa", Emoji.Wastebasket, [ Prefix.Refactor; Prefix.Wip ]

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

    let (|MinInputWithMatchingPrefixAndEmojis|) =
        function
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.C ->
            "c",
            Prefix.Chore,
            [
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
            "d",
            Prefix.Docs,
            [
                Emoji.Bulb
                Emoji.BustsInSilhouette
                Emoji.CameraFlash
                Emoji.Memo
                Emoji.MoneyWithWings
                Emoji.PageFacingUp
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.Fe ->
            "fe",
            Prefix.Feat,
            [
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
            "fi",
            Prefix.Fix,
            [
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
            "p",
            Prefix.Perf,
            [
                Emoji.Thread // ↩
                Emoji.Zap
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.Ref ->
            "ref",
            Prefix.Refactor,
            [
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
            "t",
            Prefix.Test,
            [
                Emoji.CameraFlash
                Emoji.ClownFace
                Emoji.Seedling
                Emoji.TestTube
                Emoji.WhiteCheckMark
            ]
        | MinInputToMatchExactlyOnePrefixAndManyEmojis.W ->
            "w",
            Prefix.Wip,
            [
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
    let ``start at the prefix step with all possible prefixes and emojis`` () = // ↩
        init () =! initial

module ``1_ select prefix first`` =
    [<Property>]
    let ``filter prefixes to match the given input`` (InputWithManyMatchingPrefixes(input, expectedPrefixes)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged input)

        actual
        |> shouldHave [
            CurrentStep(Step.Prefix expectedPrefixes) // ↩
            CurrentInput input
        ]

    [<Property>]
    let ``select the first prefix matching the given input when pressing [Enter]`` (InputWithManyMatchingPrefixes(input, expectedPrefixes)) =
        let expectedPrefix = expectedPrefixes.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged input)
            |> update Msg.Enter

        actual |> shouldHave [ CompleteStep(CompletedStep.Prefix expectedPrefix) ]

    [<Property>]
    let ``select the exact matching prefix`` (MinInputWithMatchingPrefixAndEmojis(exactPrefixInput, expectedPrefix, _)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)

        actual |> shouldHave [ CompleteStep(CompletedStep.Prefix expectedPrefix) ]

module ``2_ select emoji after prefix`` =
    [<Property>]
    let ``limit the selectable emojis according to the selected prefix`` (MinInputWithMatchingPrefixAndEmojis(exactPrefixInput, _, expectedEmojis)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)

        actual
        |> shouldHave [
            CurrentStep(Step.Emoji expectedEmojis) // ↩
            CurrentInput ""
        ]

    [<Property>]
    let ``select the first emoji matching the selected prefix when pressing [Enter]`` (MinInputWithMatchingPrefixAndEmojis(exactPrefixInput, expectedPrefix, expectedEmojis)) =
        let expectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.Enter

        actual
        |> shouldHave [
            CompleteStep(CompletedStep.Prefix expectedPrefix) // ↩
            CompleteStep(CompletedStep.Emoji expectedEmoji)
        ]

    [<Fact>]
    let ``select both Revert prefix and Rewind emoji when inputting 'rev'`` () =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged "rev")

        actual
        |> shouldHave [
            CompleteStep(CompletedStep.Prefix Prefix.Revert) // ↩
            CompleteStep(CompletedStep.Emoji Emoji.Rewind)
        ]

module ``3_ select emoji first`` =
    [<Fact>]
    let ``switch to emoji selection when typing the character ':'`` () =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged ":")

        actual
        |> shouldHave [
            CurrentStep(Step.Emoji Emoji.All) // ↩
            CurrentInput ""
        ]

    [<Property>]
    let ``filter emojis to match the given input`` (InputWithManyMatchingEmojis(input, expectedEmojis)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged ":")
            |> update (Msg.InputChanged input)

        actual
        |> shouldHave [
            CurrentStep(Step.Emoji expectedEmojis) // ↩
            CurrentInput input
        ]

    [<Property>]
    let ``select the first emoji matching the given input when pressing [Enter]`` (InputWithManyMatchingEmojis(input, expectedEmojis)) =
        let expectedSelectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged ":")
            |> update (Msg.InputChanged input)
            |> update Msg.Enter

        actual |> shouldHave [ CompleteStep(CompletedStep.Emoji expectedSelectedEmoji) ]

    [<Property>]
    let ``select the emoji matching exactly the given input`` (MinInputWithMatchingEmojiWithManyPrefixes(exactEmojiInput, expectedSelectedEmoji, _)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged ":")
            |> update (Msg.InputChanged exactEmojiInput)

        actual |> shouldHave [ CompleteStep(CompletedStep.Emoji expectedSelectedEmoji) ]

module ``4_ select prefix after emoji`` =
    [<Property>]
    let ``limit the selectable prefixes according to the selected emoji`` (MinInputWithMatchingEmojiWithManyPrefixes(exactEmojiInput, _, expectedPrefixes)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged ":")
            |> update (Msg.InputChanged exactEmojiInput)

        actual
        |> shouldHave [
            CurrentStep(Step.Prefix expectedPrefixes) // ↩
            CurrentInput ""
        ]

    [<Property>]
    let ``select the first prefix when pressing [Enter]`` (MinInputWithMatchingEmojiWithManyPrefixes(exactEmojiInput, expectedSelectedEmoji, expectedPrefixes)) =
        let expectedPrefix = expectedPrefixes.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged ":")
            |> update (Msg.InputChanged exactEmojiInput)
            |> update Msg.Enter

        actual
        |> shouldHave [
            CompleteStep(CompletedStep.Prefix expectedPrefix) // ↩
            CompleteStep(CompletedStep.Emoji expectedSelectedEmoji)
        ]

    [<Property>]
    let ``select directly the single possible prefix`` (MinInputWithMatchingEmojiWithSinglePrefix(exactEmojiInput, expectedSelectedEmoji, expectedPrefix)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged ":")
            |> update (Msg.InputChanged exactEmojiInput)

        actual
        |> shouldHave [
            CompleteStep(CompletedStep.Prefix expectedPrefix) // ↩
            CompleteStep(CompletedStep.Emoji expectedSelectedEmoji)
        ]

module ``5_ select breaking change`` =
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
            CompleteStep(CompletedStep.Prefix expectedPrefix) // ↩
            CompleteStep(CompletedStep.Emoji Emoji.Boom)
            CompleteStep(CompletedStep.BreakingChange { Selected = true; Disabled = true })
        ]

    [<Property>]
    let ``select the breaking change when pressing [!] given a feat or a fix`` (MinInputMatchingFeatOrFixWithEmojis(exactPrefixInput, expectedPrefix, expectedEmojis)) =
        let expectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.Enter // Select first emoji
            |> update (Msg.InputChanged "!") // Confirm breaking change

        actual
        |> shouldHave [
            CompleteStep(CompletedStep.Prefix expectedPrefix) // ↩
            CompleteStep(CompletedStep.Emoji expectedEmoji)
            CompleteStep(CompletedStep.BreakingChange { Selected = true; Disabled = false })
        ]

    [<Property>]
    let ``confirm no breaking change when pressing [Enter] given a feat or a fix`` (MinInputMatchingFeatOrFixWithEmojis(exactPrefixInput, expectedPrefix, expectedEmojis)) =
        let expectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.Enter // Select first emoji
            |> update Msg.Enter // Confirm no breaking change

        actual
        |> shouldHave [
            CompleteStep(CompletedStep.Prefix expectedPrefix) // ↩
            CompleteStep(CompletedStep.Emoji expectedEmoji)
            CompleteStep(CompletedStep.BreakingChange { Selected = false; Disabled = false })
        ]

    [<Property>]
    let ``any input is considered invalid except '!'`` (NonEmptyString invalidInput) (MinInputMatchingFeatOrFixWithEmojis(exactPrefixInput, _, _)) =
        if invalidInput <> "!" then
            let actual =
                initial // ↩
                |> update (Msg.InputChanged exactPrefixInput)
                |> update Msg.Enter // Select first emoji
                |> update (Msg.InputChanged invalidInput)

            actual
            |> shouldHave [ CurrentStep(Step.BreakingChange({ Selected = false; Disabled = false }, invalidInput = Some invalidInput)) ]

    [<Property>]
    let ``prevent selecting the breaking change given neither a feat nor a fix`` (MinInputWithMatchingEmojiWithManyPrefixesAndNoBreakingChange(exactPrefixInput, expectedPrefix, expectedEmojis)) =
        let expectedEmoji = expectedEmojis.Head

        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.Enter

        actual
        |> shouldHave [
            CompleteStep(CompletedStep.Prefix expectedPrefix) // ↩
            CompleteStep(CompletedStep.Emoji expectedEmoji)
            CompleteStep(CompletedStep.BreakingChange { Selected = false; Disabled = true })
        ]

module ``6_ determine semantic version change`` =
    [<Property>]
    let ``indicate major given a feat or a fix with breaking change`` (MinInputMatchingFeatOrFixWithEmojis(exactPrefixInput, _, _)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.Enter // Select first emoji
            |> update (Msg.InputChanged "!") // Confirm breaking change

        actual |> shouldHave [ CurrentStep(Step.Confirmation(Some SemVerChange.Major, invalidInput = None)) ]

    [<Fact>]
    let ``indicate minor given a fix with no breaking change`` () =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged "fi")
            |> update Msg.Enter // Select first emoji
            |> update Msg.Enter // Confirm no breaking change

        actual |> shouldHave [ CurrentStep(Step.Confirmation(Some SemVerChange.Minor, invalidInput = None)) ]

    [<Fact>]
    let ``indicate patch given a feat with no breaking change`` () =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged "fe")
            |> update Msg.Enter // Select first emoji
            |> update Msg.Enter // Confirm no breaking change

        actual |> shouldHave [ CurrentStep(Step.Confirmation(Some SemVerChange.Patch, invalidInput = None)) ]

    [<Property>]
    let ``indicate none given other prefix`` (MinInputWithMatchingEmojiWithManyPrefixesAndNoBreakingChange(exactPrefixInput, _, _)) =
        let actual =
            initial // ↩
            |> update (Msg.InputChanged exactPrefixInput)
            |> update Msg.Enter // Select first emoji - No possible breaking change detected

        actual |> shouldHave [ CurrentStep(Step.Confirmation(None, invalidInput = None)) ]

module ``7_ confirm selections`` =
    let private (|ModelReadyForConfirmation|) (prefix, emoji, breakingChange, semVerChange) =
        {
            initial with
                CurrentStep = {
                    Step = Step.Confirmation(semVerChange, invalidInput = None)
                    Input = ""
                    Confirmed = false
                }
                CompletedSteps = [
                    // The combination (prefix, emoji, breakingChange, semVerChange) generated by FsCheck is random,
                    // so it probably doesn't match the real relations, but it should not be important for this test
                    CompletedStep.Prefix prefix // ↩
                    CompletedStep.Emoji emoji
                    CompletedStep.BreakingChange breakingChange
                ]
        },
        (prefix, emoji, breakingChange)

    [<Property>]
    let ``any input is considered invalid at this step`` (NonEmptyString invalidInput) (ModelReadyForConfirmation(model, _)) =
        let actual = model |> update (Msg.InputChanged invalidInput)
        actual |> shouldHave [ CurrentStep(model.CurrentStep.Step |> Step.setInvalidInput invalidInput) ]

    [<Property>]
    let ``confirm all selections by pressing [Enter]`` (ModelReadyForConfirmation(model, (prefix, emoji, breakingChange))) =
        let actual = model |> update Msg.Enter
        actual |> shouldHave [ PreviousFullCompletion(Some(prefix, emoji, breakingChange)) ]