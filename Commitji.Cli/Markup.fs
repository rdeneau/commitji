module Commitji.Cli.Markup

open System

let highlight (input: string) (text: string) =
    match input.Length with
    | 0 -> text
    | n ->
        match text.IndexOf(input, StringComparison.OrdinalIgnoreCase) with
        | -1 -> text
        | i ->
            let before = text[.. i - 1]
            let after = text[i + n ..]
            $"{before}[grey19 on yellow]%s{input}[/]%s{after}"

let kbd text = $"[white on grey][[%s{text}]][/]"

let selection text = $"[bold green1]%s{text}[/]"