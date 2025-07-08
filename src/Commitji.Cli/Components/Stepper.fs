module Commitji.Cli.Components.Stepper

open System
open Commitji.Cli
open Commitji.Core.Model
open Spectre.Console
open Spectre.Console.Rendering

type StepProps = { Name: string; Status: StepStatus }

[<Literal>]
let private Separator = "  ›  "

module private Markup =
    let step index step =
        let label = $"(%i{index + 1}) %s{step.Name}"

        match step.Status with
        | Completed value -> $"%s{Markup.selectedDim label}: %s{value |> Markup.selected |> Markup.strong}"
        | Current -> label |> Markup.current |> Markup.strong
        | Pending -> label |> Markup.inactive

type Stepper =
    static member step(name, status) = // ↩
        { Name = name; Status = status }

    static member render(steps) =
        let text =
            [
                "Steps:  " |> Markup.strong |> Markup.em

                for index, step in Seq.indexed steps do
                    match index with
                    | 0 -> ()
                    | _ -> Markup.light Separator

                    Markup.step index step
            ]
            |> String.concat ""

        Markup(text + Environment.NewLine) :> IRenderable
