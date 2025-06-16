module Commitji.Cli.View

open System
open Commitji.Cli.Components.SelectionPrompt
open Commitji.Cli.Components.Stepper
open Commitji.Core
open Commitji.Core.Model
open Spectre.Console

module private Render =
    let private hintPanel hints =
        let panel =
            Panel(
                text = String.Join(Environment.NewLine, hints |> List.map (fun s -> $"[olive]• %s{s}[/]")), // ↩
                Border = BoxBorder.Rounded,
                Expand = true,
                Header = PanelHeader("[bold] 💡 Hints [/]")
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

    module private StepName =
        [<Literal>]
        let Prefix = "Prefix"

        [<Literal>]
        let Emoji = "Emoji"

        [<Literal>]
        let BreakingChange = "Breaking Change"

        [<Literal>]
        let Confirmation = "Confirmation"

        let All = Set [ Prefix; Emoji; BreakingChange; Confirmation ]

    let stepper (model: Model) =
        let existingSteps = [
            for step in List.rev model.CompletedSteps do
                match step with
                | CompletedStep.Prefix prefix -> StepName.Prefix, StepStatus.Completed prefix.Code
                | CompletedStep.Emoji emoji -> StepName.Emoji, StepStatus.Completed $"%s{emoji.Char} [grey][[%s{emoji.Code}]][/]"
                | CompletedStep.BreakingChange b -> StepName.BreakingChange, StepStatus.Completed(if b.Selected then "Yes" else "No")

            match model.CurrentStep.Step with
            | Step.Prefix _ -> StepName.Prefix, StepStatus.Current
            | Step.Emoji _ -> StepName.Emoji, StepStatus.Current
            | Step.BreakingChange _ -> StepName.BreakingChange, StepStatus.Current
            | Step.Confirmation _ -> StepName.Confirmation, StepStatus.Current
        ]

        let pendingSteps =
            Set [ for name, _ in existingSteps -> name ] // ↩
            |> Set.difference StepName.All

        Stepper.render [
            for name, status in existingSteps do
                Stepper.step (name, status)

            for name in pendingSteps do
                Stepper.step (name, StepStatus.Pending)
        ]

        AnsiConsole.WriteLine ""

    let currentStep (model: Model) =
        let input = model.CurrentStep.Input

        match model.CurrentStep.Step with
        | Step.Prefix matchingPrefixes ->
            instruction "Select a prefix for the commit message:"

            SelectionPrompt.render (currentChoiceIndex = matchingPrefixes.Index, input = input) [|
                for prefix in matchingPrefixes.Items do
                    let props = prefix.Props
                    SelectionPrompt.choice props.Code props.Hint
            |]

            AnsiConsole.WriteLine ""

            hintPanel [
                if input.Length = 0 then
                    $"""Start typing the prefix for auto-completion [grey](e.g. "fi" to select %s{Markup.selection "fix"})[/]"""

                $"""Or press %s{Markup.kbd "Up"}/%s{Markup.kbd "Down"} then %s{Markup.kbd "Enter"} to select the highlighted prefix"""

                match model.CompletedSteps with
                | [] -> $"""Press %s{Markup.kbd ":"} to start by selecting an emoji"""
                | _ when input.Length = 0 -> $"""Press %s{Markup.kbd "Backspace"} to cancel the previous selection"""
                | _ -> ()

                $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "C"} to exit"""
            ]

        | Step.Emoji matchingEmojis ->
            instruction "Select an emoji for the commit message:"

            SelectionPrompt.render (currentChoiceIndex = matchingEmojis.Index, input = input) [|
                for emoji in matchingEmojis.Items do
                    let props = emoji.Props
                    SelectionPrompt.choice props.Code $"%s{props.Char} %s{props.Hint}"
            |]

            AnsiConsole.WriteLine ""

            hintPanel [
                if input.Length = 0 then
                    $"""Start typing the emoji code for auto-completion [grey](e.g. "z" to select %s{Markup.selection "⚡"} [[zap]])[/]"""

                $"""Or press %s{Markup.kbd "Up"}/%s{Markup.kbd "Down"} then %s{Markup.kbd "Enter"} to select the highlighted emoji"""

                match model.CompletedSteps with
                | [] -> $"""Press %s{Markup.kbd ":"} to start by selecting a prefix"""
                | _ when input.Length = 0 -> $"""Press %s{Markup.kbd "Backspace"} to cancel the previous selection"""
                | _ -> ()

                $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "C"} to exit"""
            ]

        | Step.BreakingChange(_, invalidInput) ->
            instruction "Press [reverse]![/] and [reverse]Enter[/] to indicate a Breaking Change, or just [reverse]Enter[/] to skip it"

            match invalidInput with
            | Some input -> AnsiConsole.MarkupLine($"[red]Invalid input: {input}[/]")
            | None -> ()

        | Step.Confirmation(semVerChangeOption, invalidInput) ->
            AnsiConsole.MarkupLine("[bold]Confirm your changes:[/]")

            // TODO RDE
            // match semVerChangeOption with
            // | Some change -> AnsiConsole.MarkupLine($"[green]{change |> SemVerChange.toString}[/]")
            // | None -> AnsiConsole.MarkupLine("[red]No changes selected.[/]")

            match invalidInput with
            | Some input -> AnsiConsole.MarkupLine($"[red]Invalid input: {input}[/]")
            | None -> ()

        AnsiConsole.WriteLine ""
        AnsiConsole.Write input

let view (model: Model) =
    AnsiConsole.Clear()

    let title = Rule("[bold orange1]Commit[/][yellow italic]ji[/]").Centered()
    AnsiConsole.Write(title)
    AnsiConsole.WriteLine ""

    Render.stepper model
    Render.currentStep model