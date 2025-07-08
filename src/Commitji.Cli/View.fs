module Commitji.Cli.View

open System
open System.Text.RegularExpressions
open Commitji.Cli.Components
open Commitji.Cli.Components.SelectionPrompt
open Commitji.Cli.Components.Stepper
open Commitji.Core
open Commitji.Core.Model
open Commitji.Core.Model.Search
open Spectre.Console
open Spectre.Console.Rendering
open TextCopy

[<RequireQualifiedAccess>]
module private Render =
    /// 💡 This can be used to render an empty row inside a `Rows` component.
    let empty = Text.Empty :> IRenderable

    let text text = Text(text) :> IRenderable
    let markup text = Markup(text) :> IRenderable

    let rows children =
        Rows(Array.ofSeq children) :> IRenderable

[<RequireQualifiedAccess>]
module private CommitMessageTemplate =
    // To colorize #description and #explanation
    let private formatHashtags (text: string) =
        Regex.Replace(text, @"#\w+", (fun x -> Markup.applyMarkup "olive" x.Value))

    let render text =
        let grid = Grid().AddColumns(count = 2)
        grid.AddRow(Markup.current " »", formatHashtags text) :> IRenderable

[<RequireQualifiedAccess>]
module private ErrorPanel =
    let render errors =
            Rows [
                Render.empty
                Panel.errors [
                    for error in errors do
                        match error with
                        | Error.NoItems stepName ->
                            let noItemFound itemName = $"No {itemName} found."

                            match stepName with
                            | StepName.Prefix -> noItemFound "prefixes"
                            | StepName.Emoji -> noItemFound "emojis"
                            | StepName.BreakingChange -> noItemFound "breaking changes"
                            | StepName.SemVerChange
                            | StepName.Confirmation -> ()

                        | Error.InputNotSupported input -> $"Input not supported: %s{input}."
                ]
            ]

[<RequireQualifiedAccess>]
module private HintPanel =
    type Example = Example of input: string * result: string

    /// <summary>
    /// Quick way to create an <c>Example</c> with the syntax <c>input ==> result</c>.
    /// </summary>
    let private (==>) input result = // ↩
        Example(input, result)

    type private Hint with
        static member key key text = // ↩
            Hint(Markup.kbd key, text)

        static member keyStroke keyStroke text = // ↩
            Hint(Markup.keyStroke keyStroke, text)

        static member upDownKeys item =
            Hint($"""%s{Markup.kbd "↓"}/%s{Markup.kbd "↑"} + %s{Markup.kbd "Enter"}""", $"Select the next/previous %s{item}")

        static member search item operation (Example(input, result)) =
            Hint($"""%s{Markup.kbd "A"}..%s{Markup.kbd "Z"}""", $"""Start typing %s{item} for %s{Markup.selectedDim operation} [grey](e.g. "%s{input}" to select %s{Markup.selected result})[/]""")

        static member selectByNumber maxNumber item =
            Hint($"""%s{Markup.kbd "1"}..%s{Markup.kbd $"%i{maxNumber}"}""", $"Select the %s{item} by number")

    let render (model: Model) =
        Panel.hints [
            for possibility in model.AvailablePossibilities do
                match possibility, model.CurrentStep.Step with
                | Possibility.Search SearchMode.Quick, Step.Emoji _ -> Hint.search "the emoji code" "auto-completion" ("z" ==> "zap ⚡")
                | Possibility.Search SearchMode.Quick, Step.Prefix _ -> Hint.search "the prefix code" "auto-completion" ("fi" ==> "fix")

                | Possibility.Search SearchMode.FullText, Step.Emoji _ -> Hint.search "a part of the emoji code or description" "full-text search" ("typo" ==> "pencil2 ✏")
                | Possibility.Search SearchMode.FullText, Step.Prefix _ -> Hint.search "a part of the prefix code or description" "full-text search" ("bug" ==> "fix")

                | Possibility.Search _, Step.BreakingChange _ -> Hint.search "the response" "auto-completion" ("y" ==> "Yes")
                | Possibility.Search _, Step.Confirmation _ -> ()

                | Possibility.Search(SearchMode.Custom _), _ -> ()

                | Possibility.AcceptSelection, _
                | Possibility.SelectNext, _ -> () // Hints included with SelectPrevious
                | Possibility.SelectPrevious, Step.Emoji _ -> Hint.upDownKeys "emoji"
                | Possibility.SelectPrevious, Step.Prefix _ -> Hint.upDownKeys "prefix"
                | Possibility.SelectPrevious, Step.BreakingChange _ -> Hint.upDownKeys "response"
                | Possibility.SelectPrevious, Step.Confirmation _ -> ()

                | Possibility.SearchByNumber, Step.Emoji _ -> Hint.selectByNumber model.AvailableEmojis.Length "emoji"
                | Possibility.SearchByNumber, Step.Prefix _ -> Hint.selectByNumber model.AvailablePrefixes.Length "prefix"
                | Possibility.SearchByNumber, Step.BreakingChange _ -> ()
                | Possibility.SearchByNumber, Step.Confirmation _ -> ()

                | Possibility.ConfirmAllSelection, Step.Prefix _
                | Possibility.ConfirmAllSelection, Step.Emoji _
                | Possibility.ConfirmAllSelection, Step.BreakingChange _ -> ()
                | Possibility.ConfirmAllSelection, Step.Confirmation _ -> Hint.key "Enter" "Copy the commit message template to the clipboard ✅"

                | Possibility.ToggleFirstStepToEmoji, _ -> Hint.key ":" "Start by selecting an emoji"

                | Possibility.ToggleSearchMode SearchMode.Quick, _ -> Hint.keyStroke [ "Alt"; "F" ] "Exit the full-text search 🔎"
                | Possibility.ToggleSearchMode SearchMode.FullText, _ -> Hint.keyStroke [ "Alt"; "F" ] "Activate the full-text search 🔎"
                | Possibility.ToggleSearchMode(SearchMode.Custom _), _ -> ()

                | Possibility.Terminate, _ -> Hint.keyStroke [ "Ctrl"; "C" ] $"""Quit %s{Markup.error "✖"}"""
                | Possibility.Undo, _ -> Hint($"""%s{Markup.keyStroke [ "Alt"; "Z" ]} %s{Markup.em "or"} %s{Markup.kbd "Backspace"}""", "Undo the last action ⏪")
        ]

[<RequireQualifiedAccess>]
module private Instruction =
    let private render text =
        Markup(Markup.strong (Markup.current "? ") + text + Environment.NewLine) :> IRenderable

    let prefix () =
        render "Select a prefix for the commit message:"

    let emoji () =
        render "Select an emoji for the commit message:"

    let breakingChange () =
        render "Indicate if it's a breaking change:"

    let semVerChange () = // ↩
        render "Semantic version change:"

    let confirmation () = // ↩
        render "Confirm the commit message template:"

[<RequireQualifiedAccess>]
module private SemVerChange =
    let private __ = "_"
    let private up = Markup.selectedDim "+1"
    let private dot = Markup.inactive "."

    let segments semVerChange = [
        match semVerChange with
        | None -> // ↩
            SearchSegment.NotSearchable(SegmentId.Code, SegmentText "None")

        | Some(semVerChange: SemVerChange) -> // ↩
            SearchSegment.NotSearchable(SegmentId.Code, SegmentText semVerChange.Code)

            let format =
                match semVerChange with
                | Major -> up + dot + __ + dot + __
                | Minor -> __ + dot + up + dot + __
                | Patch -> __ + dot + __ + dot + up

            SearchSegment.NotSearchable(SegmentId.Hint, SegmentText format)
    ]

[<RequireQualifiedAccess>]
module Stepper =
    let private allStepNames =
        Set [ StepName.Prefix; StepName.Emoji; StepName.BreakingChange; StepName.Confirmation ]

    let determineSteps (model: Model) =
        let existingSteps = [
            for step in List.rev model.CompletedSteps do
                match step with
                | CompletedStep.Prefix prefix -> StepName.Prefix, StepStatus.Completed prefix.Item.Code
                | CompletedStep.Emoji emoji -> StepName.Emoji, StepStatus.Completed $"%s{emoji.Item.Code} %s{emoji.Item.Char}"
                | CompletedStep.BreakingChange breakingChange -> StepName.BreakingChange, StepStatus.Completed breakingChange.Item.Code
                | CompletedStep.SemVerChange _ -> ()

            match model.CurrentStep.Step with
            | Step.Prefix _ -> StepName.Prefix, StepStatus.Current
            | Step.Emoji _ -> StepName.Emoji, StepStatus.Current
            | Step.BreakingChange _ -> StepName.BreakingChange, StepStatus.Current
            | Step.Confirmation _ -> StepName.Confirmation, StepStatus.Current
        ]

        let pendingSteps =
            Set [ for name, _ in existingSteps -> name ] // ↩
            |> Set.difference allStepNames

        [
            for name, status in existingSteps do
                name, status

            for name in pendingSteps do
                name, StepStatus.Pending
        ]

    type StepName with
        member this.Text =
            match this with
            | StepName.Prefix -> "Prefix"
            | StepName.Emoji -> "Emoji"
            | StepName.BreakingChange -> "Breaking change"
            | StepName.SemVerChange -> "Semantic version change"
            | StepName.Confirmation -> "Confirmation"

    let render model =
        Render.rows [
            Stepper.render [
                for name, status in determineSteps model do
                    Stepper.step (name.Text, status)
            ]
        ]

[<RequireQualifiedAccess>]
module private CompletedSteps =
    let private renderCompletedStep instruction segments =
        Render.rows [ // ↩
            instruction ()
            SelectionPrompt.render (currentChoiceIndex = -1) [ segments |> List.filter (fun x -> x.Id <> SegmentId.Number) ]
            Render.empty
        ]

    let render (model: Model) =
        Render.rows [
            for step in List.rev model.CompletedSteps do
                match step with
                | CompletedStep.Prefix prefix -> renderCompletedStep Instruction.prefix prefix.Segments
                | CompletedStep.Emoji emoji -> renderCompletedStep Instruction.emoji emoji.Segments
                | CompletedStep.BreakingChange breakingChange -> renderCompletedStep Instruction.breakingChange breakingChange.Segments
                | CompletedStep.SemVerChange semVerChange -> renderCompletedStep Instruction.semVerChange (SemVerChange.segments semVerChange)
        ]

[<RequireQualifiedAccess>]
module private CurrentStep =
    let private renderSelectableCurrentStep model instruction (list: SelectableList<'t>) =
        Render.rows [
            instruction () :> IRenderable

            match model.Errors with
            | [] -> ()
            | errors -> ErrorPanel.render errors

            SelectionPrompt.render (currentChoiceIndex = list.Index) [ for prefix in list.Items -> prefix.Segments ]
            Render.empty
            HintPanel.render model
        ]

    let private renderConfirmationStep model commitMessageTemplate =
        Render.rows [ // ↩
            Instruction.confirmation ()
            CommitMessageTemplate.render commitMessageTemplate
            Render.empty
            HintPanel.render model
        ]

    let render (model: Model) =
        match model.CurrentStep.Step with
        | Step.Prefix prefixes -> renderSelectableCurrentStep model Instruction.prefix prefixes
        | Step.Emoji emojis -> renderSelectableCurrentStep model Instruction.emoji emojis
        | Step.BreakingChange breakingChanges -> renderSelectableCurrentStep model Instruction.breakingChange breakingChanges
        | Step.Confirmation commitMessageTemplate -> renderConfirmationStep model commitMessageTemplate

let private copyMessageToClipboard commitMessageTemplate =
    ClipboardService.SetText commitMessageTemplate
    AnsiConsole.WriteLine "✅"
    AnsiConsole.MarkupLine(Markup.selected "Copied to the clipboard.")
    AnsiConsole.WriteLine()

let private handleKeyPress (keyInfo: ConsoleKeyInfo) (model: Model) (dispatch: Msg -> unit) =
    match keyInfo.Key, keyInfo.Modifiers, keyInfo.KeyChar with
    | ConsoleKey.Backspace, _, _
    | _, (ConsoleModifiers.Control | ConsoleModifiers.Alt), 'z' -> dispatch Undo // 💡 We can use [Alt]+[Z] when [Ctrl]+[Z] is caught by the terminal
    | ConsoleKey.DownArrow, _, _ -> dispatch SelectNext
    | ConsoleKey.UpArrow, _, _ -> dispatch SelectPrevious
    | ConsoleKey.Enter, _, _ ->
        match model.CurrentStep.Step with
        | Step.Prefix _
        | Step.Emoji _
        | Step.BreakingChange _ -> dispatch AcceptSelection
        | Step.Confirmation commitMessageTemplate -> copyMessageToClipboard commitMessageTemplate
    | ConsoleKey.Escape, _, _ -> dispatch ToggleSearchMode
    | _, (ConsoleModifiers.Control | ConsoleModifiers.Alt), 'f' -> dispatch ToggleSearchMode // 💡 We can use [Alt]+[F] when [Ctrl]+[F] is caught by the terminal
    | _, ConsoleModifiers.Control, 'c' -> dispatch Terminate
    | _, _, Char.MinValue -> () // Ignore other control keys
    | _, ConsoleModifiers.None, ':' -> dispatch ToggleFirstStepToEmoji
    | _, (ConsoleModifiers.None | ConsoleModifiers.Shift), c -> dispatch (InputChanged $"%s{model.CurrentStep.Input}%c{c}")
    | _ -> ()

let readKey model dispatch =
    AnsiConsole.Console.Input.ReadKey(intercept = true)
    |> Option.ofNullable
    |> Option.iter (fun keyInfo -> handleKeyPress keyInfo model dispatch)

let render model =
    Render.rows [
        Rule("[bold orange1]Commit[/][yellow italic]ji[/]").Centered()
        Render.empty

        Stepper.render model
        CompletedSteps.render model
        CurrentStep.render model
        Render.empty

        Markup((Markup.current "» ") + "Input: " + model.CurrentStep.Input + Markup.blink "_")
    ]