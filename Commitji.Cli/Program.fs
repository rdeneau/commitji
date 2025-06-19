module Commitji.Cli.Program

open System
open Commitji.Core
open Spectre.Console

type Elmish(init, update, view) =
    let mutable model = init ()
    let mutable history = []

    let canUndo () =
        match history with
        | [] -> false
        | _ -> true

    let view model = view (canUndo ()) model

    let handle msg = // ↩
        let newModel = update msg model

        if newModel <> model then
            history <- model :: history
            model <- newModel
            view model

    let undo () =
        match history with
        | [] -> ()
        | previous :: rest ->
            history <- rest
            model <- previous
            view model

    member _.Run() =
        view model

        let mutable shouldEnd = false

        while (not shouldEnd) do
            let keyInfo = AnsiConsole.Console.Input.ReadKey(intercept = true)

            // AnsiConsole.WriteLine $"[DEBUG] Key={keyInfo.Value.Key}, Char={keyInfo.Value.KeyChar}, Modifiers={keyInfo.Value.Modifiers}"

            match Option.ofNullable keyInfo with
            | None -> shouldEnd <- true
            | Some keyInfo ->
                match keyInfo.Key, keyInfo.Modifiers, keyInfo.KeyChar with
                | ConsoleKey.Backspace, _, _
                | _, ConsoleModifiers.Control, 'z' -> undo ()
                | ConsoleKey.DownArrow, _, _ -> handle Msg.Down
                | ConsoleKey.UpArrow, _, _ -> handle Msg.Up
                | ConsoleKey.Enter, _, _ -> handle Msg.Enter
                | ConsoleKey.Escape, _, _ -> handle (Msg.ToggleFullTextSearch false)
                | _, (ConsoleModifiers.Control | ConsoleModifiers.Alt), 'f' -> handle (Msg.ToggleFullTextSearch true) // 💡 We can use [Alt]+[F] when [Ctrl]+[F] is caught by the terminal
                | _, ConsoleModifiers.Control, 'c' -> shouldEnd <- true
                | _, _, Char.MinValue -> () // Ignore other control keys
                | _, (ConsoleModifiers.None | ConsoleModifiers.Shift), c -> handle (Msg.InputChanged $"%s{model.CurrentStep.Input}%c{c}")
                | _ -> ()

[<EntryPoint>]
let main _ =
    Console.OutputEncoding <- System.Text.Encoding.UTF8 // 👈 To ensure proper display of emojis while running the executable

    let program = Elmish(State.init, State.update, View.render)
    program.Run()

    0