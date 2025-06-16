[<AutoOpen>]
module Commitji.Core.Types

open Commitji.Core.Model

[<RequireQualifiedAccess>]
type CompletedStep =
    | Prefix of selectedPrefix: Prefix
    | Emoji of selectedEmoji: Emoji // TODO: add the possibility to skip the emoji - it can be Emoji.None "-", matching "none" and linked to any Prefix...
    | BreakingChange of BreakingChange

type SelectableList<'T> =
    { Items: 'T list
      Index: int }

type SelectableList =
    static member Create(items, ?index) =
        { Items = items; Index = defaultArg index 0 }

type List<'T> with
    member this.AsSelectable =
        SelectableList.Create(this)

[<RequireQualifiedAccess>]
type Step =
    | Prefix of SelectableList<Prefix>
    | Emoji of SelectableList<Emoji>
    | BreakingChange of BreakingChange * invalidInput: string option
    | Confirmation of SemVerChange option * invalidInput: string option

type CurrentStep = {
    Step: Step
    Input: string
    Confirmed: bool
}

type Model = {
    CurrentStep: CurrentStep
    CompletedSteps: CompletedStep list
    SelectablePrefixes: Prefix list
    SelectableEmojis: Emoji list
    PreviousFullCompletion: (Prefix * Emoji * BreakingChange) option
// TODO: Backspace Key Hint Cases
// TODO: Enter Key Hint Cases
// TODO: Emoji Char Hint Optionality
// TODO: BreakingChange Char Hint Optionality
}

type Msg =
    | Backspace
    | Enter
    | Down
    | Up
    | InputChanged of input: string

[<RequireQualifiedAccess>]
module CommandChar =
    [<Literal>]
    let Emoji = ":"

    [<Literal>]
    let BreakingChange = "!"