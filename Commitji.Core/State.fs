module Commitji.Core.State

open Commitji.Core.Model

type private MatchingStrategy =
    | FirstMatchAtIndex
    | ExactMatch

let private (|Match|_|) strategy { Items = items; Index = index } =
    match strategy, items with
    | FirstMatchAtIndex, _ -> Some(items[index])
    | ExactMatch, [ item ] -> Some item
    | _ -> None

[<RequireQualifiedAccess>]
module Step =
    let setInvalidInput input step =
        match step with
        | Step.Prefix _
        | Step.Emoji _ -> failwith $"%A{step} does not hold an invalid input"
        | Step.BreakingChange(breakingChange, _) -> Step.BreakingChange(breakingChange, invalidInput = Some input)
        | Step.Confirmation(semVerChangeOption, _) -> Step.Confirmation(semVerChangeOption, invalidInput = Some input)

[<RequireQualifiedAccess>]
module private CurrentStep =
    let start (step: Step) = {
        Step = step
        Input = ""
        Confirmed = false
    }

    let setInvalidInput input (currentStep: CurrentStep) = {
        Step = Step.setInvalidInput input currentStep.Step
        Input = ""
        Confirmed = false
    }

[<RequireQualifiedAccess>]
module private SelectableList =
    let selectNext (selectableList: SelectableList<'T>) = {
        selectableList with
            Index =
                match selectableList.Index + 1 with
                | i when i >= List.length selectableList.Items -> 0 // Wrap around to the first item
                | i -> i
    }

    let selectPrevious (selectableList: SelectableList<'T>) = {
        selectableList with
            Index =
                match selectableList.Index - 1 with
                | i when i < 0 -> List.length selectableList.Items - 1 // Wrap around to the last item
                | i -> i
    }

let init () = {
    CurrentStep = CurrentStep.start (Step.Prefix Prefix.All.AsSelectable)
    CompletedSteps = []
    SelectablePrefixes = Prefix.All
    SelectableEmojis = Emoji.All
    PreviousFullCompletion = None
}

let private (|IsEmpty|IsNotEmpty|) (input: string) =
    if input.Length = 0 then IsEmpty else IsNotEmpty

let private findMatchingPrefixes (model: Model) =
    match model.CurrentStep.Input with
    | IsEmpty -> model.SelectablePrefixes
    | IsNotEmpty -> [
        for prefix in model.SelectablePrefixes do
            if prefix.Code.StartsWith(model.CurrentStep.Input, System.StringComparison.OrdinalIgnoreCase) then
                prefix
      ]

let private findMatchingEmojis (model: Model) =
    match model.CurrentStep.Input with
    | IsEmpty -> model.SelectableEmojis
    | IsNotEmpty -> [
        for emoji in model.SelectableEmojis do
            if emoji.Code.StartsWith(model.CurrentStep.Input, System.StringComparison.OrdinalIgnoreCase) then
                emoji
      ]

let private findMatches (model: Model) =
    match model.CurrentStep.Step with
    | Step.Prefix _ -> { model with Model.CurrentStep.Step = findMatchingPrefixes model |> SelectableList.Create |> Step.Prefix }
    | Step.Emoji _ -> { model with Model.CurrentStep.Step = findMatchingEmojis model |> SelectableList.Create |> Step.Emoji }
    | _ -> model

/// Restart the previous step, if any.
[<TailCall>]
let rec private rollback (model: Model) =
    match model.CompletedSteps, model.CurrentStep.Step with
    | [], Step.Emoji _ -> // ↩
        { model with CurrentStep = CurrentStep.start (Step.Prefix model.SelectablePrefixes.AsSelectable) }
    | [], _ -> model // No previous step to roll back to -> stay in the current step.
    | previousStep :: completedSteps, _ ->
        let restartAt (step: Step) = {
            model with // ↩
                CurrentStep = CurrentStep.start step
                CompletedSteps = completedSteps
        }

        match previousStep with
        | CompletedStep.Prefix _ -> restartAt (Step.Prefix model.SelectablePrefixes.AsSelectable)
        | CompletedStep.Emoji _ -> restartAt (Step.Emoji model.SelectableEmojis.AsSelectable)
        | CompletedStep.BreakingChange breakingChange ->
            let model = restartAt (Step.BreakingChange(breakingChange, invalidInput = None))

            if breakingChange.Disabled then
                // The breaking change step is not editable, so we roll back one more time
                rollback model
            else
                model

let private startEmojiStep selectableEmojis (model: Model) = {
    model with // ↩
        SelectableEmojis = selectableEmojis
        CurrentStep = CurrentStep.start (Step.Emoji selectableEmojis.AsSelectable)
}

let private startPrefixStep selectablePrefixes (model: Model) = {
    model with // ↩
        SelectablePrefixes = selectablePrefixes
        CurrentStep = CurrentStep.start (Step.Prefix selectablePrefixes.AsSelectable)
}

let private startBreakingChangeStep selectedEmoji selectedPrefix (model: Model) = {
    model with // ↩
        CurrentStep = CurrentStep.start (Step.BreakingChange(BreakingChange.determine selectedEmoji selectedPrefix, invalidInput = None))
}

let private startConfirmationStep breakingChange prefix (model: Model) = {
    model with // ↩
        CurrentStep = CurrentStep.start (Step.Confirmation(SemVerChange.determine breakingChange prefix, invalidInput = None))
}

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

let private completeFullyAndRestart (model: Model) =
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

    let emoji =
        model.CompletedSteps
        |> List.tryPick (
            function
            | CompletedStep.Emoji emoji -> Some emoji
            | _ -> None
        )
        |> function
            | Some emoji -> emoji
            | None -> failwith "Cannot complete breaking change step without a selected emoji."

    let breakingChange =
        model.CompletedSteps
        |> List.tryPick (
            function
            | CompletedStep.BreakingChange breakingChange -> Some breakingChange
            | _ -> None
        )
        |> function
            | Some breakingChange -> breakingChange
            | None -> failwith "Cannot complete confirmation step without a selected breaking change."

    { init () with PreviousFullCompletion = Some(prefix, emoji, breakingChange) }

let private tryCompleteCurrentStep strategy (model: Model) =
    match model.CurrentStep.Step with
    // Steps using the given matching strategy
    | Step.Prefix _ when model.CurrentStep.Input = CommandChar.Emoji && model.CompletedSteps = [] ->
        // Switch the start step to Emoji selection
        { model with CurrentStep = CurrentStep.start (Step.Emoji model.SelectableEmojis.AsSelectable) }
    | Step.Prefix(Match strategy selectedPrefix) -> model |> completePrefixStep selectedPrefix
    | Step.Prefix _ -> model

    | Step.Emoji(Match strategy selectedEmoji) -> model |> completeEmojiStep selectedEmoji
    | Step.Emoji _ -> model

    // Steps using the model.CurrentStep.Confirmed
    | Step.BreakingChange(breakingChange, _) when breakingChange.Disabled || model.CurrentStep.Confirmed -> // ↩
        model |> completeBreakingChangeStep breakingChange
    | Step.BreakingChange(breakingChange, _) when model.CurrentStep.Input = CommandChar.BreakingChange -> // ↩
        model |> completeBreakingChangeStep { breakingChange with Selected = true }
    | Step.BreakingChange _ when model.CurrentStep.Input <> "" -> // Invalid input -> reset
        { model with CurrentStep = model.CurrentStep |> CurrentStep.setInvalidInput model.CurrentStep.Input }
    | Step.BreakingChange _ -> model

    | Step.Confirmation _ when model.CurrentStep.Input <> "" -> // Invalid input -> reset
        { model with CurrentStep = model.CurrentStep |> CurrentStep.setInvalidInput model.CurrentStep.Input }
    | Step.Confirmation _ when model.CurrentStep.Confirmed -> // ↩
        model |> completeFullyAndRestart
    | Step.Confirmation _ -> model

[<TailCall>]
let rec private tryCompleteManySteps strategy model =
    let completedModel = tryCompleteCurrentStep strategy model

    if completedModel = model then
        completedModel // No changes -> more completion is not possible.
    else
        tryCompleteManySteps ExactMatch completedModel // Try complete more steps in a row, but only for exact matches.

let update (msg: Msg) (model: Model) =
    let model = // ↩
        { model with Model.CurrentStep.Confirmed = (msg = Enter) }

    match msg, model.CurrentStep with
    | Backspace, { Input = IsEmpty } -> model |> rollback
    | Backspace, { Input = input } -> { model with Model.CurrentStep.Input = input[.. input.Length - 2] } |> findMatches
    | InputChanged input, _ -> { model with Model.CurrentStep.Input = input } |> findMatches |> tryCompleteManySteps ExactMatch
    | Enter, _ -> model |> tryCompleteManySteps FirstMatchAtIndex
    | Down, { Step = Step.Prefix prefixes } -> { model with Model.CurrentStep.Step = prefixes |> SelectableList.selectNext |> Step.Prefix }
    | Down, { Step = Step.Emoji emojis } -> { model with Model.CurrentStep.Step = emojis |> SelectableList.selectNext |> Step.Emoji }
    | Down, _ -> model // No change for other steps
    | Up, { Step = Step.Prefix prefixes } -> { model with Model.CurrentStep.Step = prefixes |> SelectableList.selectPrevious |> Step.Prefix }
    | Up, { Step = Step.Emoji emojis } -> { model with Model.CurrentStep.Step = emojis |> SelectableList.selectPrevious |> Step.Emoji }
    | Up, _ -> model // No change for other steps
