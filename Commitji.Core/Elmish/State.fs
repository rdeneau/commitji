module Commitji.Core.State

open System
open Commitji.Core.Model
open Commitji.Core.Model.Search

type private MatchingStrategy =
    | FirstMatchAtIndex
    | ExactMatch

let private (|Match|_|) strategy ({ Items = items; Index = index }: SelectableList<'t>) =
    match strategy, items with
    | FirstMatchAtIndex, list -> Some(list[index].Item)
    | ExactMatch, [ x ] -> Some x.Item
    | _ -> None

[<RequireQualifiedAccess>]
module private CurrentStep =
    let start (step: Step) = {
        Step = step
        Input = ""
        Confirmed = false
    }

    let setInvalidInput _input (currentStep: CurrentStep) = currentStep // TODO: to implement with a Notice

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
    let private numberCodeHint (segmentsConfig: SegmentsConfiguration) index code hint =
        let segment id text =
            segmentsConfig.States // ↩
            |> Map.tryFind id
            |> Option.map (SearchSegment.create id text)

        [
            segment SegmentId.Number $"%i{index + 1}." // ↩
            segment SegmentId.Code code
            segment SegmentId.Hint hint
        ]
        |> List.choose id

    let prefix segmentsConfig index (prefix: Prefix) =
        numberCodeHint segmentsConfig index prefix.Code prefix.Hint

    let emoji segmentsConfig index (emoji: Emoji) =
        numberCodeHint segmentsConfig index emoji.Code $"%s{emoji.Char} %s{emoji.Hint}"

[<AutoOpen>]
module Extensions =
    type SegmentsConfiguration with
        member this.AsPrefixesSearch = Search(Segments.prefix this)
        member this.AsEmojisSearch = Search(Segments.emoji this)

    type Model with
        member this.SegmentsConfiguration = SegmentsConfiguration.ofSearchMode this.SearchMode

[<RequireQualifiedAccess>]
module SelectableList =
    let searchedBy input (search: Search<'t, _>) items =
        search.Run(input, items, StringComparison.OrdinalIgnoreCase) |> SelectableList.init

    let searchable (search: Search<'t, _>) items =
        search.Init(items) |> SelectableList.init

    [<RequireQualifiedAccess>]
    module Prefixes =
        let searchedBy input (segmentsConfig: SegmentsConfiguration) (prefixes: Prefix list) =
            searchedBy input segmentsConfig.AsPrefixesSearch prefixes

        let searchable (segmentsConfig: SegmentsConfiguration) (prefixes: Prefix list) =
            searchable segmentsConfig.AsPrefixesSearch prefixes

    [<RequireQualifiedAccess>]
    module Emojis =
        let searchedBy input (segmentsConfig: SegmentsConfiguration) (emojis: Emoji list) =
            searchedBy input segmentsConfig.AsEmojisSearch emojis

        let searchable (segmentsConfig: SegmentsConfiguration) (emojis: Emoji list) =
            searchable segmentsConfig.AsEmojisSearch emojis

    let selectNext (selectableList: SelectableList<'t>) = {
        selectableList with
            Index =
                match selectableList.Index + 1 with
                | i when i >= selectableList.Items.Length -> 0 // Wrap around to the first item
                | i -> i
    }

    let selectPrevious (selectableList: SelectableList<'t>) = {
        selectableList with
            Index =
                match selectableList.Index with
                | 0 -> selectableList.Items.Length - 1 // Wrap around to the last item
                | i -> i - 1
    }

let initWith searchMode =
    let segmentsConfig = SegmentsConfiguration.ofSearchMode searchMode

    {
        CurrentStep = CurrentStep.start (Step.Prefix(SelectableList.Prefixes.searchable segmentsConfig Prefix.All))
        CompletedSteps = []
        AvailablePrefixes = Prefix.All
        AvailableEmojis = Emoji.All
        AvailableBreakingChanges = BreakingChange.All
        SearchMode = searchMode
        PreviousFullCompletion = None
    }

let init () = initWith SearchMode.Quick

let private performSearch (model: Model) =
    let step =
        match model.CurrentStep.Step, SearchInput.tryCreate model.CurrentStep.Input with
        | Step.Prefix _, Some input -> // ↩
            Step.Prefix(SelectableList.Prefixes.searchedBy input model.SegmentsConfiguration model.AvailablePrefixes)

        | Step.Prefix _, None -> // ↩
            Step.Prefix(SelectableList.Prefixes.searchable model.SegmentsConfiguration model.AvailablePrefixes)

        | Step.Emoji _, Some input -> // ↩
            Step.Emoji(SelectableList.Emojis.searchedBy input model.SegmentsConfiguration model.AvailableEmojis)

        | Step.Emoji _, None -> // ↩
            Step.Emoji(SelectableList.Emojis.searchable model.SegmentsConfiguration model.AvailableEmojis)

        | step, _ -> step

    { model with Model.CurrentStep.Step = step }

[<AutoOpen>]
module private StartStep =
    let startEmojiStep selectableEmojis (model: Model) = {
        model with // ↩
            AvailableEmojis = selectableEmojis
            CurrentStep = CurrentStep.start (Step.Emoji(SelectableList.Emojis.searchable model.SegmentsConfiguration selectableEmojis))
    }

    let startPrefixStep selectablePrefixes (model: Model) = {
        model with // ↩
            AvailablePrefixes = selectablePrefixes
            CurrentStep = CurrentStep.start (Step.Prefix(SelectableList.Prefixes.searchable model.SegmentsConfiguration selectablePrefixes))
    }

    let startBreakingChangeStep selectedEmoji selectedPrefix (model: Model) =
        let tbd = BreakingChange.determine selectedEmoji selectedPrefix

        {
            model with // ↩
                CurrentStep = CurrentStep.start (Step.BreakingChange(tbd))
        }

    let startConfirmationStep breakingChange prefix (model: Model) = {
        model with // ↩
            CurrentStep = CurrentStep.start (Step.Confirmation(SemVerChange.determine breakingChange prefix, invalidInput = None))
    }

    let restartCurrentStep (model: Model) =
        match model.CurrentStep.Step with
        | Step.Prefix _ -> model |> startPrefixStep model.AvailablePrefixes
        | Step.Emoji _ -> model |> startEmojiStep model.AvailableEmojis
        | Step.BreakingChange _
        | Step.Confirmation _ -> model

[<AutoOpen>]
module private StepCompletion =
    let private addCompletedStep completedStep (model: Model) = {
        model with // ↩
            CompletedSteps = completedStep :: model.CompletedSteps
    }

    let private completePrefixStep (selectedPrefix: Prefix) (model: Model) =
        match model.CompletedSteps with
        | [] ->
            model // ↩
            |> startEmojiStep (Relation.emojisForPrefix selectedPrefix)
            |> addCompletedStep (CompletedStep.Prefix selectedPrefix)
        | [ CompletedStep.Emoji selectedEmoji ] ->
            model // ↩
            |> startBreakingChangeStep selectedEmoji selectedPrefix
            |> addCompletedStep (CompletedStep.Prefix selectedPrefix)
        | _ -> failwith $"Unexpected state: cannot complete prefix step given completed steps %A{model.CompletedSteps}."

    let private completeEmojiStep (selectedEmoji: Emoji) (model: Model) =
        match model.CompletedSteps with
        | [] ->
            model // ↩
            |> startPrefixStep (Relation.prefixesForEmoji selectedEmoji)
            |> addCompletedStep (CompletedStep.Emoji selectedEmoji)
        | [ CompletedStep.Prefix selectedPrefix ] ->
            model // ↩
            |> startBreakingChangeStep selectedEmoji selectedPrefix
            |> addCompletedStep (CompletedStep.Emoji selectedEmoji)
        | _ -> failwith $"Unexpected state: cannot complete emoji step given completed steps %A{model.CompletedSteps}."

    let private completeBreakingChangeStep breakingChange (model: Model) =
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
        |> startConfirmationStep breakingChange prefix
        |> addCompletedStep (CompletedStep.BreakingChange breakingChange)

    // TODO: Notice.AllStepsCompleted
    let private noticeAllStepsCompleted (model: Model) = model

    let private tryCompleteCurrentStep strategy (model: Model) =
        match model.CurrentStep.Step with
        // Steps using the given matching strategy
        | Step.Prefix _ when model.CurrentStep.Input = CommandChar.Emoji && model.CompletedSteps = [] ->
            // Switch the start step to Emoji selection
            { model with AvailableEmojis = Emoji.All; CurrentStep = CurrentStep.start (Step.Emoji(SelectableList.Emojis.searchable model.SegmentsConfiguration Emoji.All)) }
        | Step.Prefix(Match strategy selectedPrefix) -> model |> completePrefixStep selectedPrefix
        | Step.Prefix _ -> model

        | Step.Emoji(Match strategy selectedEmoji) -> model |> completeEmojiStep selectedEmoji
        | Step.Emoji _ -> model

        | Step.BreakingChange(Match strategy selectedBreakingChange) -> model |> completeBreakingChangeStep selectedBreakingChange
        | Step.BreakingChange _ -> model

        // Steps using the model.CurrentStep.Confirmed
        | Step.Confirmation _ when model.CurrentStep.Input <> "" -> // Invalid input -> reset
            { model with CurrentStep = model.CurrentStep |> CurrentStep.setInvalidInput model.CurrentStep.Input }
        | Step.Confirmation _ when model.CurrentStep.Confirmed -> // ↩
            model |> noticeAllStepsCompleted
        | Step.Confirmation _ -> model

    [<TailCall>]
    let rec tryCompleteManySteps strategy model =
        let completedModel = tryCompleteCurrentStep strategy model

        if completedModel = model then
            completedModel // No changes -> more completion is not possible.
        else
            tryCompleteManySteps ExactMatch completedModel // Try complete more steps in a row, but only for exact matches.

let rec update (msg: Msg) (model: Model) =
    let model = // ↩
        { model with Model.CurrentStep.Confirmed = (msg = Enter) }

    match msg, model.CurrentStep with
    | InputChanged input, _ -> { model with Model.CurrentStep.Input = input } |> performSearch |> tryCompleteManySteps ExactMatch
    | Enter, _ -> model |> tryCompleteManySteps FirstMatchAtIndex
    | Down, { Step = Step.Prefix prefixes } -> { model with Model.CurrentStep.Step = prefixes |> SelectableList.selectNext |> Step.Prefix }
    | Up, { Step = Step.Prefix prefixes } -> { model with Model.CurrentStep.Step = prefixes |> SelectableList.selectPrevious |> Step.Prefix }
    | Down, { Step = Step.Emoji emojis } -> { model with Model.CurrentStep.Step = emojis |> SelectableList.selectNext |> Step.Emoji }
    | Up, { Step = Step.Emoji emojis } -> { model with Model.CurrentStep.Step = emojis |> SelectableList.selectPrevious |> Step.Emoji }
    | Down, { Step = Step.BreakingChange breakingChanges } -> { model with Model.CurrentStep.Step = breakingChanges |> SelectableList.selectNext |> Step.BreakingChange }
    | Up, { Step = Step.BreakingChange breakingChanges } -> { model with Model.CurrentStep.Step = breakingChanges |> SelectableList.selectPrevious |> Step.BreakingChange }
    | (Down | Up), _ -> model // No change for other steps
    | ToggleFullTextSearch isFullText, _ ->
        { model with SearchMode = if isFullText then SearchMode.FullText else SearchMode.Quick }
        |> restartCurrentStep
        |> performSearch
        |> tryCompleteManySteps ExactMatch