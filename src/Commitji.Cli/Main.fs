module Commitji.Cli.Main

open System
open Commitji.Cli.Elmish
open Commitji.Core
open Spectre.Console
open Spectre.Console.Rendering

type private MainView() =
    member val Content: IRenderable = Text.Empty with get, set

    interface IRenderable with
        member this.Measure(options, maxWidth) = this.Content.Measure(options, maxWidth)
        member this.Render(options, maxWidth) = this.Content.Render(options, maxWidth)

[<EntryPoint>]
let main _ =
    Console.OutputEncoding <- System.Text.Encoding.UTF8 // 👈 To ensure proper display of emojis while running the executable
    Console.BackgroundColor <- ConsoleColor.Black
    Console.ForegroundColor <- ConsoleColor.White

    let mainView = MainView()

    AnsiConsole
        .Live(mainView)
        .AutoClear(true)
        .Start(fun ctx ->
            AnsiConsole.Console.Clear()

            let view model dispatch =
                mainView.Content <- View.render model
                ctx.Refresh()
                View.readKey model dispatch

            Program.mkProgram State.init State.update view
            |> Program.withTermination ((=) Msg.Terminate) ignore
            |> Program.run
        )

    0