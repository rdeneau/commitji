module Commitji.Cli.Main

open System
open Commitji.Cli.Elmish
open Commitji.Core

[<EntryPoint>]
let main _ =
    Console.BackgroundColor <- ConsoleColor.Black
    Console.ForegroundColor <- ConsoleColor.White
    Console.OutputEncoding <- System.Text.Encoding.UTF8 // 👈 To ensure proper display of emojis while running the executable

    // TODO: verifier si l'on ne pourrait pas utiliser le Elmish de Fable plutôt que de le réécrire
    Program.mkProgram State.init State.update View.render
    |> Program.withTermination ((=) Msg.Terminate) ignore
    |> Program.run

    0