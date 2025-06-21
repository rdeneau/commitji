module Commitji.Cli.View

open System
open Commitji.Cli.Components.SelectionPrompt
open Commitji.Cli.Components.Stepper
open Commitji.Core
open Commitji.Core.Helpers
open Commitji.Core.Model
open Commitji.Core.Model.Search
open Spectre.Console

type StepName =
    | Prefix
    | Emoji
    | BreakingChange
    | Confirmation

[<RequireQualifiedAccess>]
module Stepper =
    [<RequireQualifiedAccess>]
    module private StepName =
        let All = Set [ Prefix; Emoji; BreakingChange; Confirmation ]

    let determineSteps (model: Model) =
        let existingSteps = [
            for step in List.rev model.CompletedSteps do
                match step with
                | CompletedStep.Prefix prefix -> StepName.Prefix, StepStatus.Completed prefix.Code
                | CompletedStep.Emoji emoji -> StepName.Emoji, StepStatus.Completed $"%s{emoji.Code} %s{emoji.Char}"
                | CompletedStep.BreakingChange breakingChange -> StepName.BreakingChange, StepStatus.Completed breakingChange.Code

            match model.CurrentStep.Step with
            | Step.Prefix _ -> StepName.Prefix, StepStatus.Current
            | Step.Emoji _ -> StepName.Emoji, StepStatus.Current
            | Step.BreakingChange _ -> StepName.BreakingChange, StepStatus.Current
            | Step.Confirmation(semVerChangeOption, _) ->
                // ⚠️ The step is directly Completed to indicate the semantic version change.
                let semVer =
                    match semVerChangeOption with
                    | Some change -> change.Code
                    | None -> "None"

                StepName.Confirmation, StepStatus.Completed semVer
        ]

        let pendingSteps =
            Set [ for name, _ in existingSteps -> name ] // ↩
            |> Set.difference StepName.All

        [
            for name, status in existingSteps do
                name, status

            for name in pendingSteps do
                name, StepStatus.Pending
        ]

    type StepName with
        member this.Text =
            match this with
            | Prefix -> "Prefix"
            | Emoji -> "Emoji"
            | BreakingChange -> "Breaking change"
            | Confirmation -> "Semantic version change"

    let render model =
        Stepper.render [
            for name, status in determineSteps model do
                Stepper.step (name.Text, status)
        ]

        AnsiConsole.WriteLine ""

module private Render =
    let private hintPanel hints =
        let panel =
            Panel(
                text = String.Join(Environment.NewLine, hints |> List.map (fun s -> $"[olive]• %s{s}[/]")), // ↩
                Border = BoxBorder.Rounded,
                Expand = true,
                Header = PanelHeader("💡[bold italic] Hints[/]")
            )

        AnsiConsole.Write(panel)

    let private selectionPanel selection =
        let panel =
            Panel(
                text = String.Join(Environment.NewLine, selection |> List.map (fun (itemType, itemText) -> $"• %s{itemType}: [green1]%s{itemText}[/]")), // ↩
                Border = BoxBorder.Rounded,
                Expand = true,
                Header = PanelHeader("[bold green] ✔ Selection [/]")
            )

        AnsiConsole.Write(panel)

    let private instruction text =
        AnsiConsole.MarkupLine($"[bold cyan]?[/] [bold]%s{text}[/]")

    let currentStep canUndo (model: Model) =
        let input = model.CurrentStep.Input

        // TODO: remove duplication between steps -> create a common component?
        match model.CurrentStep.Step with
        | Step.Prefix prefixes ->
            instruction "Select a prefix for the commit message:"

            SelectionPrompt.render (currentChoiceIndex = prefixes.Index) [
                for prefix in prefixes.Items do
                    prefix.Segments
            ]

            AnsiConsole.WriteLine ""

            hintPanel [
                match model.SearchMode with
                | SearchMode.Quick ->
                    $"""[italic]Quick search:[/] Start typing the prefix code for auto-completion [grey](e.g. "fi" to select %s{Markup.selected "fix"})[/]"""
                    $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "F"} (or %s{Markup.kbd "Alt"}+%s{Markup.kbd "F"}) to activate the full-text search"""
                | SearchMode.FullText ->
                    $"""[italic]Full-text search:[/] Start typing a part of the prefix code or hint to search [grey](e.g. "bug" to select %s{Markup.selected "fix"})[/]"""
                    $"""Press %s{Markup.kbd "Escape"} to return to the quick search"""
                | SearchMode.Custom _ -> ()

                "Or type the prefix number"

                $"""Press %s{Markup.kbd "Up"}/%s{Markup.kbd "Down"} then %s{Markup.kbd "Enter"} to select the highlighted prefix"""

                match model.CompletedSteps, input with
                | [], String.IsEmpty -> $"""Press %s{Markup.kbd ":"} to start by selecting an emoji"""
                | _ -> ()

                if canUndo then
                    $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "Z"} or %s{Markup.kbd "Backspace"} to undo the last action"""

                $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "C"} to exit"""
            ]

        | Step.Emoji emojis ->
            instruction "Select an emoji for the commit message:"

            SelectionPrompt.render (currentChoiceIndex = emojis.Index) [
                for emoji in emojis.Items do
                    emoji.Segments
            ]

            AnsiConsole.WriteLine ""

            hintPanel [
                match model.SearchMode with
                | SearchMode.Quick ->
                    $"""[italic]Quick search:[/] Start typing the emoji code for auto-completion [grey](e.g. "z" to select %s{Markup.selected "⚡"} [[zap]])[/]"""
                    $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "F"} (or %s{Markup.kbd "Alt"}+%s{Markup.kbd "F"}) to activate the full-text search"""
                | SearchMode.FullText ->
                    $"""[italic]Full-text search:[/] Start typing a part of the emoji code or hint to search [grey](e.g. "bug" to select %s{Markup.selected "fix"})[/]"""
                    $"""Press %s{Markup.kbd "Escape"} to return to the quick search"""
                | SearchMode.Custom _ -> ()

                "Or type the emoji number"

                $"""Press %s{Markup.kbd "Up"}/%s{Markup.kbd "Down"} then %s{Markup.kbd "Enter"} to select the highlighted emoji"""

                if canUndo then
                    $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "Z"} or %s{Markup.kbd "Backspace"} to undo the last action"""

                $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "C"} to exit"""
            ]

        | Step.BreakingChange breakingChanges ->
            instruction "Indicate if it's a breaking change:"

            SelectionPrompt.render (currentChoiceIndex = breakingChanges.Index) [
                for breakingChanges in breakingChanges.Items do
                    breakingChanges.Segments
            ]

            AnsiConsole.WriteLine ""

            hintPanel [
                if input.Length = 0 then
                    $"""Start typing the response for auto-completion [grey](e.g. "y" to select %s{Markup.selected "Yes"})[/]"""

                $"""Or press %s{Markup.kbd "Up"}/%s{Markup.kbd "Down"} then %s{Markup.kbd "Enter"} to select the response"""

                match model.CompletedSteps with
                | [] -> ()
                | _ when input.Length = 0 -> $"""Press %s{Markup.kbd "Backspace"} to restart the previous step"""
                | _ -> ()

                if canUndo then
                    $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "Z"} or %s{Markup.kbd "Backspace"} to undo the last action"""

                $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "C"} to exit"""
            ]

        | Step.Confirmation(_, invalidInput) ->
            instruction "Confirm your selection"
            AnsiConsole.WriteLine ""

            // TODO RDE: display the emoji and prefix hints
            // TODO RDE: display the commit message

            hintPanel [
                $"""Press %s{Markup.kbd "Backspace"} to restart the previous step"""
                $"""Press %s{Markup.kbd "Enter"} to confirm""" // TODO RDE: copy to clipboard
                $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "C"} to exit"""
            ]

            // TODO: display for all steps when using the Notice, in a panel
            match invalidInput with
            | Some input -> AnsiConsole.MarkupLine($"[red]Invalid input: {input}[/]")
            | None -> ()

        AnsiConsole.WriteLine ""
        AnsiConsole.Write input

let render canUndo model =
    AnsiConsole.Clear()

    let title = Rule("[bold orange1]Commit[/][yellow italic]ji[/]").Centered()
    AnsiConsole.Write(title)
    AnsiConsole.WriteLine ""

    Stepper.render model
    Render.currentStep canUndo model