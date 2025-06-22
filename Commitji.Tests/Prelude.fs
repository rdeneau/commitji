module Commitji.Tests.FsCheckExtensions

open Commitji.Core
open Commitji.Core.Model
open FsCheck
open FsCheck.FSharp

[<RequireQualifiedAccess>]
module private Gen =
    let mapFilterOption (g: Gen<'T option>) : Gen<'T> =
        g |> Gen.filter Option.isSome |> Gen.map Option.get

let private replayCompletedSteps steps model =
    (model, steps)
    ||> List.fold (fun model step ->
        match step with
        | CompletedStep.Prefix prefix ->
            match model.CurrentStep.Step with
            | Step.Prefix _ -> model |> State.update (Msg.InputChanged prefix.Code)
            | _ -> model
        | CompletedStep.Emoji emoji ->
            match model.CurrentStep.Step with
            | Step.Emoji _ -> model |> State.update (Msg.InputChanged emoji.Code)
            | _ -> model
        | CompletedStep.BreakingChange breakingChange ->
            match model.CurrentStep.Step with
            | Step.BreakingChange _ -> model |> State.update (Msg.InputChanged breakingChange.Code)
            | _ -> model
    )

module PrefixFirst =
    type PrefixSelectedOnly = PrefixSelectedOnly of Prefix * Model
    type PrefixEmojiSelectedOnly = PrefixEmojiSelectedOnly of Prefix * Emoji * Model
    type PrefixEmojiBreakingChangeSelected = PrefixEmojiBreakingChangeSelected of Prefix * Emoji * BreakingChange * SemVerChange option * Model

    let initial = State.init ()

    type Arbitraries =
        static member PrefixSelectedOnly() : Arbitrary<PrefixSelectedOnly> =
            gen {
                let! (prefix: Prefix) = ArbMap.defaults |> ArbMap.generate

                let model =
                    initial
                    |> replayCompletedSteps [ // ↩
                        CompletedStep.Prefix prefix
                    ]

                return
                    match model.CurrentStep.Step with
                    | Step.Emoji _ -> Some(PrefixSelectedOnly(prefix, model))
                    | _ -> None
            }
            |> Gen.mapFilterOption
            |> Arb.fromGen

        static member PrefixEmojiSelectedOnly() : Arbitrary<PrefixEmojiSelectedOnly> =
            gen {
                let! (prefix: Prefix) = ArbMap.defaults |> ArbMap.generate
                let! (emoji: Emoji) = ArbMap.defaults |> ArbMap.generate

                let model =
                    initial
                    |> replayCompletedSteps [ // ↩
                        CompletedStep.Prefix prefix
                        CompletedStep.Emoji emoji
                    ]

                return
                    match model.CurrentStep.Step with
                    | Step.BreakingChange _ ->
                        // Pick the actual emoji: generated or directly auto-completed from the prefix
                        let emoji =
                            model.CompletedSteps
                            |> List.tryPick (
                                function
                                | CompletedStep.Emoji emoji -> Some emoji
                                | _ -> None
                            )

                        match emoji with
                        | Some emoji -> Some(PrefixEmojiSelectedOnly(prefix, emoji, model))
                        | None -> None
                    | _ -> None
            }
            |> Gen.mapFilterOption
            |> Arb.fromGen

        static member PrefixEmojiBreakingChangeSelected() : Arbitrary<PrefixEmojiBreakingChangeSelected> =
            gen {
                let! (prefix: Prefix) = ArbMap.defaults |> ArbMap.generate
                let! (emoji: Emoji) = ArbMap.defaults |> ArbMap.generate
                let! (breakingChange: BreakingChange) = ArbMap.defaults |> ArbMap.generate

                let model =
                    initial
                    |> replayCompletedSteps [ // ↩
                        CompletedStep.Prefix prefix
                        CompletedStep.Emoji emoji
                        CompletedStep.BreakingChange breakingChange
                    ]

                return
                    match model.CurrentStep.Step with
                    | Step.Confirmation(semVer, _) ->
                        // Pick the actual emoji: generated or directly auto-completed from the prefix
                        let emoji =
                            model.CompletedSteps
                            |> List.tryPick (
                                function
                                | CompletedStep.Emoji emoji -> Some emoji
                                | _ -> None
                            )

                        // Pick the actual breaking change: generated or directly auto-completed from the emoji & prefix
                        let breakingChange =
                            model.CompletedSteps
                            |> List.tryPick (
                                function
                                | CompletedStep.BreakingChange breakingChange -> Some breakingChange
                                | _ -> None
                            )

                        match emoji, breakingChange with
                        | Some emoji, Some breakingChange -> Some(PrefixEmojiBreakingChangeSelected(prefix, emoji, breakingChange, semVer, model))
                        | _ -> None
                    | _ -> None
            }
            |> Gen.mapFilterOption
            |> Arb.fromGen

module EmojiFirst =
    type EmojiSelectedOnly = EmojiSelectedOnly of Emoji * Model
    type EmojiPrefixSelectedOnly = EmojiPrefixSelectedOnly of Emoji * Prefix * Model
    type EmojiPrefixBreakingChangeSelected = EmojiPrefixBreakingChangeSelected of Emoji * Prefix * BreakingChange * SemVerChange option * Model

    let initial = State.init () |> State.update Msg.ToggleFirstStepToEmoji

    type Arbitraries =
        static member EmojiSelectedOnly() : Arbitrary<EmojiSelectedOnly> =
            gen {
                let! (emoji: Emoji) = ArbMap.defaults |> ArbMap.generate

                let model =
                    initial
                    |> replayCompletedSteps [ // ↩
                        CompletedStep.Emoji emoji
                    ]

                return
                    match model.CurrentStep.Step with
                    | Step.Prefix _ -> Some(EmojiSelectedOnly(emoji, model))
                    | _ -> None
            }
            |> Gen.mapFilterOption
            |> Arb.fromGen

        static member EmojiPrefixSelectedOnly() : Arbitrary<EmojiPrefixSelectedOnly> =
            gen {
                let! (prefix: Prefix) = ArbMap.defaults |> ArbMap.generate
                let! (emoji: Emoji) = ArbMap.defaults |> ArbMap.generate

                let model =
                    initial
                    |> replayCompletedSteps [ // ↩
                        CompletedStep.Emoji emoji
                        CompletedStep.Prefix prefix
                    ]

                return
                    match model.CurrentStep.Step with
                    | Step.BreakingChange _ ->
                        // Pick the actual prefix: generated or directly auto-completed from the emoji
                        let prefix =
                            model.CompletedSteps
                            |> List.tryPick (
                                function
                                | CompletedStep.Prefix prefix -> Some prefix
                                | _ -> None
                            )

                        match prefix with
                        | Some prefix -> Some(EmojiPrefixSelectedOnly(emoji, prefix, model))
                        | None -> None
                    | _ -> None
            }
            |> Gen.mapFilterOption
            |> Arb.fromGen

        static member EmojiPrefixBreakingChangeSelected() : Arbitrary<EmojiPrefixBreakingChangeSelected> =
            gen {
                let! (emoji: Emoji) = ArbMap.defaults |> ArbMap.generate
                let! (prefix: Prefix) = ArbMap.defaults |> ArbMap.generate
                let! (breakingChange: BreakingChange) = ArbMap.defaults |> ArbMap.generate

                let model =
                    initial
                    |> replayCompletedSteps [ // ↩
                        CompletedStep.Emoji emoji
                        CompletedStep.Prefix prefix
                        CompletedStep.BreakingChange breakingChange
                    ]

                return
                    match model.CurrentStep.Step with
                    | Step.Confirmation(semVer, _) ->
                        // Pick the actual prefix: generated or directly auto-completed from the emoji
                        let prefix =
                            model.CompletedSteps
                            |> List.tryPick (
                                function
                                | CompletedStep.Prefix prefix -> Some prefix
                                | _ -> None
                            )

                        // Pick the actual breaking change: generated or directly auto-completed from the emoji & prefix
                        let breakingChange =
                            model.CompletedSteps
                            |> List.tryPick (
                                function
                                | CompletedStep.BreakingChange breakingChange -> Some breakingChange
                                | _ -> None
                            )

                        match prefix, breakingChange with
                        | Some prefix, Some breakingChange -> Some(EmojiPrefixBreakingChangeSelected(emoji, prefix, breakingChange, semVer, model))
                        | _ -> None
                    | _ -> None
            }
            |> Gen.mapFilterOption
            |> Arb.fromGen