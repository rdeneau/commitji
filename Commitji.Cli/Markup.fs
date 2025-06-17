module Commitji.Cli.Markup

open System

let highlight (input: string) (text: string) =
    match input.Length with
    | 0 -> text
    | n ->
        let rec loop (current: string) fragments =
            match current.IndexOf(input, StringComparison.OrdinalIgnoreCase) with
            | -1 -> (String.concat "" fragments) + current
            | i ->
                let before = current[.. i - 1]
                loop current[i + n ..] (before :: $"[grey19 on yellow]%s{current[i .. i + n - 1]}[/]" :: fragments)

        loop text []

let kbd text = $"[white on grey][[%s{text}]][/]"

let selection text = $"[bold green1]%s{text}[/]"