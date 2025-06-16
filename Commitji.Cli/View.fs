module Commitji.Cli.View

open System
open Commitji.Core
open Commitji.Core.Model
open Spectre.Console

module private Markup =
    let highlight text = $"[bold white on blue]%s{text}[/]"
    let kbd text = $"[white on grey][[%s{text}]][/]"

    let promptChoice isCurrent code hint =
        if isCurrent then
            [| $" [cyan]»[/] [green1]%s{code}[/]"; $"[cyan]%s{hint}[/]" |]
        else
            [| $"   [green4]%s{code}[/]"; hint |]

module private Render =
    let grid (rows: string array list) =
        let grid = Grid().AddColumns(rows[0].Length)

        for row in rows do
            grid.AddRow(row) |> ignore

        AnsiConsole.Write(grid)

    let highlight text =
        AnsiConsole.Markup(Markup.highlight text)

    let instruction text =
        AnsiConsole.MarkupLine($"[bold cyan]?[/] [bold]%s{text}[/]")

    let hintPanel (hints: string list) =
        let panel =
            Panel(
                text = String.Join(Environment.NewLine, hints |> List.map (fun s -> $"[olive]• %s{s}[/]")), // ↩
                Border = BoxBorder.Rounded,
                Expand = true,
                Header = PanelHeader("[bold] 💡 Hints [/]")
            )

        AnsiConsole.Write(panel)

let view (model: Model) =
    AnsiConsole.Clear()

    let title = Rule("[bold orange1]Commit[/][yellow italic]ji[/]").Centered()
    AnsiConsole.Write(title)
    AnsiConsole.WriteLine ""

    let input = model.CurrentStep.Input

    match model.CurrentStep.Step with
    | Step.Prefix matchingPrefixes ->
        Render.instruction "Select a prefix for the commit message:"

        Render.grid [
            for i, prefix in List.indexed matchingPrefixes.Items do
                let props = prefix.Props
                let code = $"[grey19 on yellow]%s{input}[/]%s{props.Code[input.Length..]}"
                Markup.promptChoice (i = matchingPrefixes.Index) code props.Hint
        ]

        AnsiConsole.WriteLine ""

        Render.hintPanel [
            if input.Length = 0 then
                $"""Start typing the prefix for auto-completion [grey](e.g. "fi" to select %s{Markup.highlight "fix"})[/]"""
            $"""Or press %s{Markup.kbd "↓"} or %s{Markup.kbd "↑"}, then %s{Markup.kbd "Enter"} to select the highlighted prefix"""
            $"""Press %s{Markup.kbd "Ctrl"}+%s{Markup.kbd "C"} to exit"""
        ]

    | Step.Emoji matchingEmojis -> AnsiConsole.MarkupLine("[bold]Select an emoji:[/]")

    // TODO RDE
    // for emoji in matchingEmojis do
    //     AnsiConsole.MarkupLine($"[green]{emoji.Code}[/]")

    | Step.BreakingChange(_, invalidInput) ->
        Render.instruction "Press [reverse]![/] and [reverse]Enter[/] to indicate a Breaking Change, or just [reverse]Enter[/] to skip it"

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

    for step in model.CompletedSteps do
        match step with
        | CompletedStep.Prefix selectedPrefix ->
            Render.highlight $"%s{selectedPrefix.Code}:"
            AnsiConsole.Write " "
        | CompletedStep.Emoji selectedEmoji ->
            Render.highlight $"%s{selectedEmoji.Char}"
            AnsiConsole.Write " "
        | CompletedStep.BreakingChange { Selected = true } ->
            Render.highlight "!"
            AnsiConsole.Write " "
        | CompletedStep.BreakingChange _ -> ()

    AnsiConsole.WriteLine ""
    AnsiConsole.Write input

    ()