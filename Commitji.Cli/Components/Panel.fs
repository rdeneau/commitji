namespace Commitji.Cli.Components

open Commitji.Cli
open Spectre.Console

type Hint = Hint of title: string * text: string

[<RequireQualifiedAccess>]
module private Markup =
    let hint text = Markup.applyMarkup "olive" text

type Panel =
    static member private panel(icon, header, content) =
        Panel(
            content = content, // ↩
            Border = BoxBorder.Rounded,
            Expand = true,
            Header = PanelHeader($"""%s{icon}%s{Markup.strong (Markup.em header)}""")
        )
        |> AnsiConsole.Write

    static member errors errors =
        let title =
            match errors with
            | [ _ ] -> "Error"
            | _ -> "Errors"

        let content =
            Rows(children = [| for error in errors -> Markup(Markup.error error) |])

        Panel.panel ("❗ ", title, content)

    static member hints hints =
        let grid = Grid().AddColumns(2)

        for Hint(title, text) in hints do
            grid.AddRow(title, Markup.hint text) |> ignore

        Panel.panel ("💡 ", "Hints", grid)