module Commitji.Core.Program

open Commitji.Core.Model
open Elmish.WPF

[<RequireQualifiedAccess>]
type CompletedStep =
    | Prefix of selectedPrefix: Prefix
    | Emoji of selectedEmoji: Emoji // TODO: add the possibility to skip the emoji - it can be Emoji.None "-", matching "none" and linked to any Prefix...
    | BreakingChange of BreakingChange

[<RequireQualifiedAccess>]
type Step =
    | Prefix of matchingPrefixes: Prefix list
    | Emoji of matchingEmojis: Emoji list
    | BreakingChange of BreakingChange * invalidInput: string option
    | Confirmation of SemVerChange option * invalidInput: string option

type CurrentStep = {
    Step: Step
    Input: string
    Confirmed: bool
}

type Model = {
    CurrentStep: CurrentStep
    CompletedSteps: CompletedStep list
    SelectablePrefixes: Prefix list
    SelectableEmojis: Emoji list
    PreviousFullCompletion: (Prefix * Emoji * BreakingChange) option
// TODO: Backspace Key Hint Cases
// TODO: Enter Key Hint Cases
// TODO: Emoji Char Hint Optionality
// TODO: BreakingChange Char Hint Optionality
}

type Msg =
    | Backspace
    | Enter
    | InputChanged of input: string

[<RequireQualifiedAccess>]
module CommandChar =
    [<Literal>]
    let Emoji = ":"

    [<Literal>]
    let BreakingChange = "!"

[<AutoOpen>]
module State =
    type private MatchingStrategy =
        | FirstMatch
        | ExactMatch

    let private (|Match|_|) strategy items =
        match strategy, items with
        | FirstMatch, item :: _ -> Some item
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

    let init () = {
        CurrentStep = CurrentStep.start (Step.Prefix Prefix.All)
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
                let props = Prefix.props prefix

                if props.Code.StartsWith(model.CurrentStep.Input, System.StringComparison.OrdinalIgnoreCase) then
                    prefix
          ]

    let private findMatchingEmojis (model: Model) =
        match model.CurrentStep.Input with
        | IsEmpty -> model.SelectableEmojis
        | IsNotEmpty -> [
            for emoji in model.SelectableEmojis do
                let props = Emoji.props emoji

                if props.Code.StartsWith(model.CurrentStep.Input, System.StringComparison.OrdinalIgnoreCase) then
                    emoji
          ]

    let private findMatches (model: Model) =
        match model.CurrentStep.Step with
        | Step.Prefix _ -> { model with Model.CurrentStep.Step = Step.Prefix(findMatchingPrefixes model) }
        | Step.Emoji _ -> { model with Model.CurrentStep.Step = Step.Emoji(findMatchingEmojis model) }
        | _ -> model

    /// Restart the previous step, if any.
    [<TailCall>]
    let rec private rollback (model: Model) =
        match model.CompletedSteps, model.CurrentStep.Step with
        | [], Step.Emoji _ -> // ↩
            { model with CurrentStep = CurrentStep.start (Step.Prefix model.SelectablePrefixes) }
        | [], _ -> model // No previous step to roll back to -> stay in the current step.
        | previousStep :: completedSteps, _ ->
            let restartAt (step: Step) = {
                model with // ↩
                    CurrentStep = CurrentStep.start step
                    CompletedSteps = completedSteps
            }

            match previousStep with
            | CompletedStep.Prefix _ -> restartAt (Step.Prefix model.SelectablePrefixes)
            | CompletedStep.Emoji _ -> restartAt (Step.Emoji model.SelectableEmojis)
            | CompletedStep.BreakingChange breakingChange ->
                let model = restartAt (Step.BreakingChange(breakingChange, invalidInput = None))

                if breakingChange.Disabled then
                    // The breaking change step is not editable, so we roll back one more time
                    rollback model
                else
                    model

    let private startEmojiStep selectableEmojis (model: Model) = {
        model with // ↩
            CurrentStep = CurrentStep.start (Step.Emoji selectableEmojis)
            SelectableEmojis = selectableEmojis
    }

    let private startPrefixStep selectablePrefixes (model: Model) = {
        model with // ↩
            CurrentStep = CurrentStep.start (Step.Prefix selectablePrefixes)
            SelectablePrefixes = selectablePrefixes
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
            { model with CurrentStep = CurrentStep.start (Step.Emoji model.SelectableEmojis) }
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
            tryCompleteManySteps strategy completedModel // Try complete more steps in a row.

    let update (msg: Msg) (model: Model) =
        let model = { model with Model.CurrentStep.Confirmed = (msg = Enter) }

        match msg, model.CurrentStep with
        | Backspace, { Input = IsEmpty } -> model |> rollback
        | Backspace, _ -> model // Let the TextBox handle the backspace and change the input.
        | Enter, _ -> model |> tryCompleteManySteps FirstMatch
        | InputChanged input, _ -> { model with Model.CurrentStep.Input = input } |> findMatches |> tryCompleteManySteps ExactMatch

let bindings () : Binding<Model, Msg> list = []
// "CounterValue" |> Binding.oneWay (fun m -> m.Count)
// "Increment" |> Binding.cmd Increment
// "Decrement" |> Binding.cmd Decrement
// "StepSize" |> Binding.twoWay ((fun m -> float m.StepSize), int >> SetStepSize)
// "Reset" |> Binding.cmdIf (Reset, canReset)

let designVm = ViewModel.designInstance (init ()) (bindings ())

let main window =
    Program.mkSimpleWpf init update bindings // ↩
    |> Program.startElmishLoop ElmConfig.Default window