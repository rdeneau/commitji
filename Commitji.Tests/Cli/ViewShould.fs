module Commitji.Tests.Cli.ViewShould

open Commitji.Cli.View
open Commitji.Core.Model
open Commitji.Tests.FsCheckExtensions
open FsCheck.Xunit
open Swensen.Unquote
open global.Xunit

module ``determine steps in the stepper`` =
    [<Properties(Arbitrary = [| typeof<PrefixFirst.Arbitraries> |])>]
    module ``1_ starting selection by prefix`` =
        open PrefixFirst

        [<Fact>]
        let ``1_ [Prefix] > Emoji > BreakingChange > Confirmation`` () =
            let actual = Stepper.determineSteps initial

            actual
            =! [ // ↩
                StepName.Prefix, StepStatus.Current
                StepName.Emoji, StepStatus.Pending
                StepName.BreakingChange, StepStatus.Pending
                StepName.SemVerChange, StepStatus.Pending
                StepName.Confirmation, StepStatus.Pending
            ]

        [<Property>]
        let ``2_ Prefix✔️ > [Emoji] > BreakingChange > Confirmation`` (PrefixSelectedOnly(prefix, model)) =
            let actual = Stepper.determineSteps model

            actual
            =! [ // ↩
                StepName.Prefix, StepStatus.Completed prefix.Code
                StepName.Emoji, StepStatus.Current
                StepName.BreakingChange, StepStatus.Pending
                StepName.SemVerChange, StepStatus.Pending
                StepName.Confirmation, StepStatus.Pending
            ]

        [<Property>]
        let ``3_ Prefix✔️ > Emoji✔️ > [BreakingChange] > Confirmation`` (PrefixEmojiSelectedOnly(prefix, emoji, model)) =
            let actual = Stepper.determineSteps model

            actual
            =! [ // ↩
                StepName.Prefix, StepStatus.Completed prefix.Code
                StepName.Emoji, StepStatus.Completed $"%s{emoji.Code} %s{emoji.Char}"
                StepName.BreakingChange, StepStatus.Current
                StepName.Confirmation, StepStatus.Pending
            ]

        [<Property(MaxTest = 1000)>]
        let ``4_ Prefix✔️ > Emoji✔️ > BreakingChange✔️ > [Confirmation]`` (PrefixEmojiBreakingChangeSelected(prefix, emoji, breakingChange, model)) =
            let actual = Stepper.determineSteps model

            actual
            =! [ // ↩
                StepName.Prefix, StepStatus.Completed prefix.Code
                StepName.Emoji, StepStatus.Completed $"%s{emoji.Code} %s{emoji.Char}"
                StepName.BreakingChange, StepStatus.Completed breakingChange.Code
                StepName.Confirmation, StepStatus.Current
            ]

    [<Properties(Arbitrary = [| typeof<EmojiFirst.Arbitraries> |])>]
    module ``2_ starting selection by emoji`` =
        open EmojiFirst

        [<Fact>]
        let ``1_ [Emoji] > Prefix > BreakingChange > Confirmation`` () =
            let actual = Stepper.determineSteps initial

            actual
            =! [ // ↩
                StepName.Emoji, StepStatus.Current
                StepName.Prefix, StepStatus.Pending
                StepName.BreakingChange, StepStatus.Pending
                StepName.Confirmation, StepStatus.Pending
            ]

        [<Property>]
        let ``2_ Emoji✔️ > [Prefix] > BreakingChange > Confirmation`` (EmojiSelectedOnly(emoji, model)) =
            let actual = Stepper.determineSteps model

            actual
            =! [ // ↩
                StepName.Emoji, StepStatus.Completed $"%s{emoji.Code} %s{emoji.Char}"
                StepName.Prefix, StepStatus.Current
                StepName.BreakingChange, StepStatus.Pending
                StepName.Confirmation, StepStatus.Pending
            ]

        [<Property>]
        let ``3_ Emoji✔️ > Prefix✔️ > [BreakingChange] > Confirmation`` (EmojiPrefixSelectedOnly(emoji, prefix, model)) =
            let actual = Stepper.determineSteps model

            actual
            =! [ // ↩
                StepName.Emoji, StepStatus.Completed $"%s{emoji.Code} %s{emoji.Char}"
                StepName.Prefix, StepStatus.Completed prefix.Code
                StepName.BreakingChange, StepStatus.Current
                StepName.Confirmation, StepStatus.Pending
            ]

        [<Property(MaxTest = 1000)>]
        let ``4_ Emoji✔️ > Prefix✔️ > BreakingChange✔️ > [Confirmation]`` (EmojiPrefixBreakingChangeSelected(emoji, prefix, breakingChange, model)) =
            let actual = Stepper.determineSteps model

            actual
            =! [ // ↩
                StepName.Emoji, StepStatus.Completed $"%s{emoji.Code} %s{emoji.Char}"
                StepName.Prefix, StepStatus.Completed prefix.Code
                StepName.BreakingChange, StepStatus.Completed breakingChange.Code
                StepName.Confirmation, StepStatus.Current
            ]