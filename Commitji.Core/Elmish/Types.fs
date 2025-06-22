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
    | Confirmation of commitMessageTemplate: string

type CurrentStep = {
    Step: Step
    Input: string
}

[<RequireQualifiedAccess>]
type CompletedStep =
    | Prefix of selectedPrefix: SearchItem<Prefix>
    | Emoji of selectedEmoji: SearchItem<Emoji> // TODO 💡 skip the emoji - it can be Emoji.None "-", matching "none" and linked to any Prefix...
    | BreakingChange of selectedBreakingChange: SearchItem<BreakingChange>
    | SemVerChange of SemVerChange option

[<RequireQualifiedAccess>]
type Possibility =
    | AcceptSelection
    | ConfirmAllSelection
    | SelectNext
    | SelectPrevious
    | Search of SearchMode
    | SearchByNumber // TODO ❗ fix SearchByNumber
    | ToggleFirstStepToEmoji
    | ToggleSearchMode of SearchMode
    | Terminate
    | Undo
    // TODO 💡 ToggleHints: show: '?', hide: [Echap]

[<RequireQualifiedAccess>]
type Error = // ↩
    | NoItems of StepName
    | InputNotSupported of input: string

type Model = {
    CurrentStep: CurrentStep
    CompletedSteps: CompletedStep list

    /// Remark: depend on the eventual emoji selected
    AvailablePrefixes: Prefix list

    /// Remark: depend on the eventual prefix selected
    AvailableEmojis: Emoji list

    /// Remark: depend on the eventual prefix & emoji selected
    AvailableBreakingChanges: BreakingChange list

    /// Remark: depend on the whole state
    AvailablePossibilities: Possibility list

    SearchMode: SearchMode
    History: Model list
    Errors: Error list
}

type Msg =
    | AcceptSelection
    | ConfirmAllSelection
    | SelectNext
    | SelectPrevious
    | InputChanged of input: string
    | ToggleFirstStepToEmoji
    | ToggleSearchMode
    | Terminate
    | Undo