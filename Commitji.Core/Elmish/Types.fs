[<AutoOpen>]
module Commitji.Core.Types

open Commitji.Core.Model
open Commitji.Core.Model.Search

type SegmentsConfiguration = { States: Map<SegmentId, SegmentState> }

[<RequireQualifiedAccess>]
type SearchMode =
    | Quick
    | FullText
    | Custom of label: string * segments: SegmentsConfiguration

[<RequireQualifiedAccess>]
type Step =
    | Prefix of SelectableList<Prefix>
    | Emoji of SelectableList<Emoji>
    | BreakingChange of SelectableList<BreakingChange>
    | Confirmation of SemVerChange option * invalidInput: string option

type CurrentStep = {
    Step: Step
    Input: string
    Confirmed: bool
}

[<RequireQualifiedAccess>]
type CompletedStep =
    | Prefix of selectedPrefix: Prefix
    | Emoji of selectedEmoji: Emoji // TODO: add the possibility to skip the emoji - it can be Emoji.None "-", matching "none" and linked to any Prefix...
    | BreakingChange of BreakingChange

[<RequireQualifiedAccess>]
type FirstStep =
    | Prefix
    | Emoji

type Model = {
    CurrentStep: CurrentStep
    CompletedSteps: CompletedStep list

    /// Remark: depend on the eventual emoji selected
    AvailablePrefixes: Prefix list

    /// Remark: depend on the eventual prefix selected
    AvailableEmojis: Emoji list

    /// Remark: depend on the eventual prefix & emoji selected
    AvailableBreakingChanges: BreakingChange list

    SearchMode: SearchMode

    // TODO: to remove once the copy to clipboard is implemented
    PreviousFullCompletion: (Prefix * Emoji * BreakingChange) option

// TODO: Backspace Key Hint Cases
// TODO: Enter Key Hint Cases
// TODO: Emoji Char Hint Optionality
// TODO: BreakingChange Char Hint Optionality
}

type Msg =
    | AcceptSelection
    | SelectNext
    | SelectPrevious
    | InputChanged of input: string
    | ToggleFirstStepToEmoji
    | ToggleSearchMode

// TODO: handle Notices
type Notice =
    | NoNotice
    | InvalidInput of input: string
    | AllStepsCompleted of commitMessageTemplate: string