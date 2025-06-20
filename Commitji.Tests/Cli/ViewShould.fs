module Commitji.Tests.Cli.ViewShould

open Commitji.Cli.Components.Stepper
open Commitji.Cli.View
open Commitji.Core
open Commitji.Core.Model
open FsCheck.Xunit
open Swensen.Unquote
open global.Xunit

module ``determine steps in the stepper`` =
    module ``starting selection by prefix`` =
        let initial = State.init ()

        [<Fact>]
        let ``1_ [Prefix] > Emoji > BreakingChange > Confirmation`` () =
            let actual = Stepper.determineSteps initial

            actual
            =! [ // ↩
                StepName.Prefix, StepStatus.Current
                StepName.Emoji, StepStatus.Pending
                StepName.BreakingChange, StepStatus.Pending
                StepName.Confirmation, StepStatus.Pending
            ]

        let (|PrefixSelectedOnly|) (prefix: Prefix) =
            let model = initial |> State.update (Msg.InputChanged prefix.Code)

            let ok =
                match model.CurrentStep.Step with
                | Step.Emoji _ -> true
                | _ -> false

            ok, prefix, model

        [<Property>]
        let ``2_ Prefix✔️ > [Emoji] > BreakingChange > Confirmation`` (PrefixSelectedOnly(ok, prefix, model)) =
            if ok then
                let actual = Stepper.determineSteps model

                actual
                =! [ // ↩
                    StepName.Prefix, StepStatus.Completed prefix.Code
                    StepName.Emoji, StepStatus.Current
                    StepName.BreakingChange, StepStatus.Pending
                    StepName.Confirmation, StepStatus.Pending
                ]

        let (|PrefixEmojiSelectedOnly|) (prefix: Prefix, emoji: Emoji) =
            let result =
                (initial, [ prefix.Code; emoji.Code ]) // ↩
                ||> List.scan (fun model input -> model |> State.update (Msg.InputChanged input))
                |> List.tryPick (fun model ->
                    match model.CurrentStep.Step with
                    | Step.BreakingChange _ ->
                        model.CompletedSteps
                        |> List.tryPick (
                            function
                            | CompletedStep.Emoji emoji -> Some (emoji, model)
                            | _ -> None
                        )
                    | _ -> None
                )

            match result with
            | None -> false, prefix, emoji, initial
            | Some(emoji, model) -> true, prefix, emoji, model

        [<Property>]
        let ``3_ Prefix✔️ > Emoji✔️ > [BreakingChange] > Confirmation`` (PrefixEmojiSelectedOnly(ok, prefix, emoji, model)) =
            if ok then
                let actual = Stepper.determineSteps model

                actual
                =! [ // ↩
                    StepName.Prefix, StepStatus.Completed prefix.Code
                    StepName.Emoji, StepStatus.Completed $"%s{emoji.Code} %s{emoji.Char}"
                    StepName.BreakingChange, StepStatus.Current
                    StepName.Confirmation, StepStatus.Pending
                ]

        let (|PrefixEmojiBreakingChangeSelected|) (prefix: Prefix, emoji: Emoji, breakingChange: bool) =
            let result =
                (initial, [ prefix.Code; emoji.Code ]) // ↩
                ||> List.scan (fun model input -> model |> State.update (Msg.InputChanged input))
                |> List.tryPick (fun model ->
                    match model.CurrentStep.Step with
                    | Step.BreakingChange _ ->
                        model.CompletedSteps
                        |> List.tryPick (
                            function
                            | CompletedStep.Emoji emoji -> Some (emoji, model)
                            | _ -> None
                        )
                    | _ -> None
                )

            match result with
            | None -> false, prefix, emoji, initial
            | Some(emoji, model) -> true, prefix, emoji, model

        [<Property>]
        let ``4_ Prefix✔️ > Emoji✔️ > BreakingChange✔️ > [Confirmation]`` (PrefixEmojiSelectedOnly(ok, prefix, emoji, model)) =
            if ok then
                let actual = Stepper.determineSteps model

                actual
                =! [ // ↩
                    StepName.Prefix, StepStatus.Completed prefix.Code
                    StepName.Emoji, StepStatus.Completed $"%s{emoji.Code} %s{emoji.Char}"
                    StepName.BreakingChange, StepStatus.Current
                    StepName.Confirmation, StepStatus.Pending
                ]