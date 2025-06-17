module Commitji.Cli.Program

open System
open Commitji.Core
open Spectre.Console

let mutable private model = State.init ()

let private dispatch msg = // ↩
    model <- State.update msg model

let private run () =
    let mutable shouldEnd = false

    while (not shouldEnd) do
        View.view model

        let keyInfo = AnsiConsole.Console.Input.ReadKey(intercept = true)

        match Option.ofNullable keyInfo with
        | None -> shouldEnd <- true
        | Some keyInfo ->
            match keyInfo.Key, keyInfo.KeyChar, keyInfo.Modifiers with
            | ConsoleKey.Backspace, _, _ -> dispatch Msg.Backspace
            | ConsoleKey.DownArrow, _, _ -> dispatch Msg.Down
            | ConsoleKey.UpArrow, _, _ -> dispatch Msg.Up
            | ConsoleKey.Enter, _, _ -> dispatch Msg.Enter
            | _, 'c', ConsoleModifiers.Control -> shouldEnd <- true
            | _, Char.MinValue, _ -> () // Ignore other control keys
            | _, c, (ConsoleModifiers.None | ConsoleModifiers.Shift) -> dispatch (Msg.InputChanged $"%s{model.CurrentStep.Input}%c{c}")
            | _ -> ()

[<EntryPoint>]
let main _ =
    run ()
    0