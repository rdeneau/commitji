module Commitji.Cli.Main

open System
open Commitji.Core
open Elmish

[<EntryPoint>]
let main _ =
    Console.BackgroundColor <- ConsoleColor.Black
    Console.ForegroundColor <- ConsoleColor.White
    Console.OutputEncoding <- System.Text.Encoding.UTF8 // 👈 To ensure proper display of emojis while running the executable

    Program.mkSimple State.init State.update View.render
    |> Program.withTermination ((=) Msg.Terminate) ignore
    |> Program.run

    0