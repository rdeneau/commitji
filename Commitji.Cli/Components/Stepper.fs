module Commitji.Cli.Components.Stepper

open Spectre.Console

type StepStatus =
    | Completed of value: string
    | Current
    | Pending

type StepProps = { Name: string; Status: StepStatus }

module private Markup =
    let step step =
        match step.Status with
        | Completed value -> $"[green1]✔[/] %s{step.Name}: [green1]%s{value}[/]"
        | Current -> $"[cyan]» %s{step.Name}[/]"
        | Pending -> $"[grey]• %s{step.Name}[/]"

type Stepper =
    static member step(name, status) = { Name = name; Status = status }

    static member render(steps) =
        AnsiConsole.Write(
            Columns [|
                "[bold italic]Steps:[/]"
                for step in steps do
                    Markup.step step
            |]
        )