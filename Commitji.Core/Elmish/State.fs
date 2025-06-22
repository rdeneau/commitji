module Commitji.Core.State

open System
open Commitji.Core.Model
open Commitji.Core.Model.Search

type private MatchingStrategy =
    | FirstMatchAtIndex
    | ExactMatch

let private (|Match|_|) strategy ({ Items = items; Index = index }: SelectableList<'t>) =
    match strategy, items with
    | FirstMatchAtIndex, list -> Some(list[index])
    | ExactMatch, [ x ] -> Some x
    | _ -> None

[<RequireQualifiedAccess>]
module private CurrentStep =
    let start (step: Step) = { Step = step; Input = "" }

[<RequireQualifiedAccess>]
module private SegmentsConfiguration =
    let private segmentsConfiguration states : SegmentsConfiguration = // ↩
        { States = Map states }

    let quickSearch =
        segmentsConfiguration [
            SegmentId.Number, SegmentState.Searchable SearchOperation.StartsWith
            SegmentId.Code, SegmentState.Searchable SearchOperation.StartsWith
            SegmentId.Hint, SegmentState.NotSearchable
        ]

    let fullTextSearch =
        segmentsConfiguration [
            SegmentId.Number, SegmentState.Searchable SearchOperation.StartsWith
            SegmentId.Code, SegmentState.Searchable SearchOperation.Contains
            SegmentId.Hint, SegmentState.Searchable SearchOperation.Contains
        ]

    let ofSearchMode searchMode =
        match searchMode with
        | SearchMode.Quick -> quickSearch
        | SearchMode.FullText -> fullTextSearch
        | SearchMode.Custom(_, segmentsConfig) -> segmentsConfig

[<RequireQualifiedAccess>]
module private Segments =
    let private applyConfig (segmentsConfig: SegmentsConfiguration) segments = [
        for segmentId, segmentText in segments do
            match Map.tryFind segmentId segmentsConfig.States with
            | Some segmentState -> SearchSegment.create segmentId segmentText segmentState
            | None -> ()
    ]

    let private toNum index = $"%i{index + 1}."

    let prefix segmentsConfig index (prefix: Prefix) =
        applyConfig segmentsConfig [ // ↩
            SegmentId.Number, index |> toNum
            SegmentId.Code, prefix.Code
            SegmentId.Hint, prefix.Hint
        ]

    let emoji segmentsConfig index (emoji: Emoji) =
        applyConfig segmentsConfig [ // ↩
            SegmentId.Number, index |> toNum
            SegmentId.Code, emoji.Code
            SegmentId.Hint, $"%s{emoji.Char} %s{emoji.Hint}"
        ]

    let breakingChange segmentsConfig _ (breakingChange: BreakingChange) =
        applyConfig segmentsConfig [ // ↩
            SegmentId.Code, breakingChange.Code
        ]

[<AutoOpen>]
module Extensions =
    type SegmentsConfiguration with
        member this.AsPrefixesSearch = Search(Segments.prefix this)
        member this.AsEmojisSearch = Search(Segments.emoji this)
        member this.AsBreakingChangesSearch = Search(Segments.breakingChange this)

    type Model with
        member this.SegmentsConfiguration = SegmentsConfiguration.ofSearchMode this.SearchMode

    type SearchMode with
        member this.Toggle() =
            match this with
            | SearchMode.Quick -> SearchMode.FullText
            | SearchMode.FullText -> SearchMode.Quick
            | SearchMode.Custom _ -> this

    type SelectableList<'t> with
        member this.SelectNext() = {
            this with
                Index =
                    match this.Index + 1 with
                    | i when i >= this.Items.Length -> 0 // Wrap around to the first item
                    | i -> i
        }

        member this.SelectPrevious() = {
            this with
                Index =
                    match this.Index with
                    | 0 -> this.Items.Length - 1 // Wrap around to the last item
                    | i -> i - 1
        }

    type Step with
        member this.Name =
            match this with
            | Step.Prefix _ -> StepName.Prefix
            | Step.Emoji _ -> StepName.Emoji
            | Step.BreakingChange _ -> StepName.BreakingChange
            | Step.Confirmation -> StepName.Confirmation

        member this.Type =
            match this with
            | Step.Prefix prefixes -> StepType.Selection prefixes.Items.Length
            | Step.Emoji emojis -> StepType.Selection emojis.Items.Length
            | Step.BreakingChange breakingChanges -> StepType.Selection breakingChanges.Items.Length
            | Step.Confirmation -> StepType.Confirmation

[<RequireQualifiedAccess>]
module SelectableList =
    let searchedBy input (search: Search<'t, _>) items =
        search.Run(input, items, StringComparison.OrdinalIgnoreCase) |> SelectableList.init

    let searchable (search: Search<'t, _>) items =
        search.Init(items) |> SelectableList.init

    [<RequireQualifiedAccess>]
    module Prefixes =
        let searchedBy input (prefixes: Prefix list) (segmentsConfig: SegmentsConfiguration) =
            searchedBy input segmentsConfig.AsPrefixesSearch prefixes

        let searchable (prefixes: Prefix list) (segmentsConfig: SegmentsConfiguration) =
            searchable segmentsConfig.AsPrefixesSearch prefixes

    [<RequireQualifiedAccess>]
    module Emojis =
        let searchedBy input (emojis: Emoji list) (segmentsConfig: SegmentsConfiguration) =
            searchedBy input segmentsConfig.AsEmojisSearch emojis

        let searchable (emojis: Emoji list) (segmentsConfig: SegmentsConfiguration) =
            searchable segmentsConfig.AsEmojisSearch emojis

    [<RequireQualifiedAccess>]
    module BreakingChanges =
        let searchedBy input (breakingChanges: BreakingChange list) (segmentsConfig: SegmentsConfiguration) =
            searchedBy input segmentsConfig.AsBreakingChangesSearch breakingChanges

        let searchable (breakingChanges: BreakingChange list) (segmentsConfig: SegmentsConfiguration) =
            searchable segmentsConfig.AsBreakingChangesSearch breakingChanges

[<AutoOpen>]
module Possibilities =
    let checkMessageAdequacyWithCurrentPossibilities (msg: Msg) (model: Model) =
        let acceptedPossibilities =
            match msg with
            | AcceptSelection -> [ Possibility.AcceptSelection ]
            | ConfirmAllSelection -> [ Possibility.ConfirmAllSelection ]
            | SelectNext -> [ Possibility.SelectNext ]
            | SelectPrevious -> [ Possibility.SelectPrevious ]
            | InputChanged _ -> [ Possibility.Search model.SearchMode; Possibility.SearchByNumber ]
            | ToggleSearchMode -> [ Possibility.ToggleSearchMode(model.SearchMode.Toggle()) ]
            | ToggleFirstStepToEmoji -> [ Possibility.ToggleFirstStepToEmoji ]
            | Terminate -> [ Possibility.Terminate ]
            | Undo -> [ Possibility.Undo ]

        acceptedPossibilities |> List.exists (fun p -> model.AvailablePossibilities |> List.contains p)

    let determinePossibilities (model: Model) =
        let step = model.CurrentStep.Step

        [
            match step.Name with
            | StepName.Prefix
            | StepName.Emoji ->
                Possibility.Search model.SearchMode

                match step.Type with
                | StepType.Selection n when n > 1 -> Possibility.SearchByNumber
                | _ -> ()

                Possibility.ToggleSearchMode(model.SearchMode.Toggle())

            | StepName.BreakingChange -> // ↩
                Possibility.Search model.SearchMode

            | StepName.SemVerChange -> ()

            | StepName.Confirmation -> // ↩
                Possibility.ConfirmAllSelection

            match step.Type with
            | StepType.Selection n when n > 1 ->
                Possibility.AcceptSelection
                Possibility.SelectNext
                Possibility.SelectPrevious

            | StepType.Selection _
            | StepType.Confirmation -> ()

            if step.Name = StepName.Prefix && model.CurrentStep.Input.Length = 0 && model.CompletedSteps.IsEmpty then
                Possibility.ToggleFirstStepToEmoji

            if not model.History.IsEmpty then
                Possibility.Undo

            Possibility.Terminate
        ]

    let definePossibilities (model: Model) = {
        model with // ↩
            AvailablePossibilities = determinePossibilities model
    }

let initWith searchMode =
    {
        CurrentStep = CurrentStep.start (Step.Prefix(SelectableList.Prefixes.searchable Prefix.All (SegmentsConfiguration.ofSearchMode searchMode)))
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

let init () = initWith SearchMode.Quick

let private performSearch (model: Model) =
    let updateModelStep getStep getSelectableList =
        let selectableList: SelectableList<_> =
            getSelectableList model.SegmentsConfiguration

        let step: Step = // ↩
            getStep selectableList

        let errors =
            match selectableList.Items with
            | [] -> [ Error.NoItems step.Name ]
            | _ -> []

        {
            model with // ↩
                Model.CurrentStep.Step = step
                Model.Errors = model.Errors @ errors
        }

    match model.CurrentStep.Step, SearchInput.tryCreate model.CurrentStep.Input with
    | Step.Prefix _, Some input -> // ↩
        updateModelStep Step.Prefix (SelectableList.Prefixes.searchedBy input model.AvailablePrefixes)

    | Step.Prefix _, None -> // ↩
        updateModelStep Step.Prefix (SelectableList.Prefixes.searchable model.AvailablePrefixes)

    | Step.Emoji _, Some input -> // ↩
        updateModelStep Step.Emoji (SelectableList.Emojis.searchedBy input model.AvailableEmojis)

    | Step.Emoji _, None -> // ↩
        updateModelStep Step.Emoji (SelectableList.Emojis.searchable model.AvailableEmojis)

    | Step.BreakingChange _, Some input -> // ↩
        updateModelStep Step.BreakingChange (SelectableList.BreakingChanges.searchedBy input model.AvailableBreakingChanges)

    | Step.BreakingChange _, None -> // ↩
        updateModelStep Step.BreakingChange (SelectableList.BreakingChanges.searchable model.AvailableBreakingChanges)

    | Step.Confirmation, Some input -> // ↩
        { model with Model.CurrentStep.Input = ""; Model.Errors = Error.InputNotSupported input.Value :: model.Errors }

    | Step.Confirmation, None -> model

[<AutoOpen>]
module private StartStep =
    let addCompletedStep completedStep (model: Model) = // ↩
        { model with CompletedSteps = completedStep :: model.CompletedSteps }

    let private startStep step (model: Model) = // ↩
        { model with CurrentStep = CurrentStep.start step }

    let startEmojiStep selectableEmojis (model: Model) =
        { model with AvailableEmojis = selectableEmojis }
        |> startStep (Step.Emoji(SelectableList.Emojis.searchable selectableEmojis model.SegmentsConfiguration))

    let startPrefixStep selectablePrefixes (model: Model) =
        { model with AvailablePrefixes = selectablePrefixes }
        |> startStep (Step.Prefix(SelectableList.Prefixes.searchable selectablePrefixes model.SegmentsConfiguration))

    let startBreakingChangeStep selectedEmoji selectedPrefix (model: Model) =
        model |> startStep (Step.BreakingChange(BreakingChange.searchable selectedEmoji selectedPrefix))

    let startConfirmationStep (model: Model) = // ↩
        model |> startStep Step.Confirmation

    let restartCurrentStep (model: Model) =
        match model.CurrentStep.Step with
        | Step.Prefix _ -> model |> startPrefixStep model.AvailablePrefixes
        | Step.Emoji _ -> model |> startEmojiStep model.AvailableEmojis
        | Step.BreakingChange _
        | Step.Confirmation -> model

[<AutoOpen>]
module private StepCompletion =
    let private completePrefixStep (selectedPrefix: SearchItem<Prefix>) (model: Model) =
        match model.CompletedSteps with
        | [] ->
            model // ↩
            |> startEmojiStep (Relation.emojisForPrefix selectedPrefix.Item)
            |> addCompletedStep (CompletedStep.Prefix selectedPrefix)
        | [ CompletedStep.Emoji selectedEmoji ] ->
            model // ↩
            |> startBreakingChangeStep selectedEmoji.Item selectedPrefix.Item
            |> addCompletedStep (CompletedStep.Prefix selectedPrefix)
        | _ -> failwith $"Unexpected state: cannot complete prefix step given completed steps %A{model.CompletedSteps}."

    let private completeEmojiStep (selectedEmoji: SearchItem<Emoji>) (model: Model) =
        match model.CompletedSteps with
        | [] ->
            model // ↩
            |> startPrefixStep (Relation.prefixesForEmoji selectedEmoji.Item)
            |> addCompletedStep (CompletedStep.Emoji selectedEmoji)
        | [ CompletedStep.Prefix selectedPrefix ] ->
            model // ↩
            |> startBreakingChangeStep selectedEmoji.Item selectedPrefix.Item
            |> addCompletedStep (CompletedStep.Emoji selectedEmoji)
        | _ -> failwith $"Unexpected state: cannot complete emoji step given completed steps %A{model.CompletedSteps}."

    let private completeBreakingChangeStep (breakingChange: SearchItem<BreakingChange>) (model: Model) =
        let prefix =
            model.CompletedSteps
            |> List.tryPick (
                function
                | CompletedStep.Prefix prefix -> Some prefix
                | _ -> None
            )
            |> function
                | Some prefix -> prefix
                | None -> failwith "Cannot complete breaking change step without a selected prefix."

        model // ↩
        |> startConfirmationStep
        |> addCompletedStep (CompletedStep.BreakingChange breakingChange)
        |> addCompletedStep (CompletedStep.SemVerChange(SemVerChange.determine breakingChange.Item prefix.Item))

    let private tryCompleteCurrentStep strategy (model: Model) =
        match model.CurrentStep.Step with
        // Steps using the given matching strategy
        | Step.Prefix(Match strategy selectedPrefix) -> model |> completePrefixStep selectedPrefix
        | Step.Prefix _ -> model
        | Step.Emoji(Match strategy selectedEmoji) -> model |> completeEmojiStep selectedEmoji
        | Step.Emoji _ -> model
        | Step.BreakingChange(Match strategy selectedBreakingChange) -> model |> completeBreakingChangeStep selectedBreakingChange
        | Step.BreakingChange _ -> model

        // The final step cannot be auto-completed, but it can be confirmed, with `Msg.ConfirmAllSelection`
        | Step.Confirmation -> model

    [<TailCall>]
    let rec tryCompleteManySteps strategy model =
        let completedModel = tryCompleteCurrentStep strategy model

        if completedModel = model then
            completedModel // No changes -> more completion is not possible.
        else
            tryCompleteManySteps ExactMatch completedModel // Try complete more steps in a row, but only for exact matches.

    // TODO: Notice.AllStepsCompleted
    let noticeAllStepsCompleted (model: Model) = model

let private handleMessage (msg: Msg) (model: Model) =
    match msg, model.CurrentStep with
    | InputChanged input, _ -> { model with Model.CurrentStep.Input = input } |> performSearch |> tryCompleteManySteps ExactMatch
    | AcceptSelection, _ -> model |> tryCompleteManySteps FirstMatchAtIndex
    | SelectNext, { Step = Step.Emoji emojis } -> { model with Model.CurrentStep.Step = Step.Emoji(emojis.SelectNext()) }
    | SelectNext, { Step = Step.Prefix prefixes } -> { model with Model.CurrentStep.Step = Step.Prefix(prefixes.SelectNext()) }
    | SelectNext, { Step = Step.BreakingChange breakingChanges } -> { model with Model.CurrentStep.Step = Step.BreakingChange(breakingChanges.SelectNext()) }
    | SelectPrevious, { Step = Step.Emoji emojis } -> { model with Model.CurrentStep.Step = Step.Emoji(emojis.SelectPrevious()) }
    | SelectPrevious, { Step = Step.Prefix prefixes } -> { model with Model.CurrentStep.Step = Step.Prefix(prefixes.SelectPrevious()) }
    | SelectPrevious, { Step = Step.BreakingChange breakingChanges } -> { model with Model.CurrentStep.Step = Step.BreakingChange(breakingChanges.SelectPrevious()) }
    | (SelectNext | SelectPrevious), _ -> model // No change for other steps
    | ToggleSearchMode, _ ->
        { model with SearchMode = model.SearchMode.Toggle() }
        |> restartCurrentStep
        |> performSearch
        |> tryCompleteManySteps ExactMatch
    | ToggleFirstStepToEmoji, { Step = Step.Prefix _ } -> model |> startEmojiStep Emoji.All
    | ToggleFirstStepToEmoji, _ -> model
    | ConfirmAllSelection, { Step = Step.Confirmation } -> model |> noticeAllStepsCompleted
    | ConfirmAllSelection, _ -> model
    | Terminate, _ -> model // Termination is handled by the Elmish program
    | Undo, _ -> model // Undo msg is handled before: in the `update` function

let update (msg: Msg) (model: Model) =
    if not (checkMessageAdequacyWithCurrentPossibilities msg model) then
        model
    else
        match msg, model.History with
        | Undo, previous :: _ -> previous
        | Undo, [] -> model
        | _ ->
            { model with History = model :: model.History; Errors = [] } // ↩
            |> handleMessage msg
            |> definePossibilities