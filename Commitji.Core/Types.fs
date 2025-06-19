[<AutoOpen>]
module Commitji.Core.Types

open Commitji.Core.Model
open Commitji.Core.Model.Search

[<RequireQualifiedAccess>]
type SegmentId =
    | Number
    | Code
    | Hint

[<RequireQualifiedAccess>]
module SegmentId =
    let All = Set [ SegmentId.Number; SegmentId.Code; SegmentId.Hint ]

type SegmentsConfiguration = { States: Map<SegmentId, SegmentState> }

[<RequireQualifiedAccess>]
type SearchMode =
    | Quick
    | FullText
    | Custom of label: string * segments: SegmentsConfiguration

[<RequireQualifiedAccess>]
type SelectableItems<'t> =
    | Searchable of SearchableList<'t, SegmentId>
    | Searched of SearchableList<'t, SegmentId>

/// Represents a list of items where we can select one item: the item with the given index.
type SelectableList<'t> =
    { Items: SelectableItems<'t>; Index: int }

    member this.Head =
        match this.Items with
        | SelectableItems.Searchable items -> items.Head.Item
        | SelectableItems.Searched items -> items.Head.Item

    member this.ItemAt index =
        match this.Items with
        | SelectableItems.Searchable items -> items[index].Item
        | SelectableItems.Searched items -> items[index].Item

    member this.Length =
        match this.Items with
        | SelectableItems.Searchable items -> items.Length
        | SelectableItems.Searched items -> items.Length

type SelectableList =
    static member init items = // ↩
        { Items = items; Index = 0 }

[<RequireQualifiedAccess>]
type Step =
    | Prefix of SelectableList<Prefix>
    | Emoji of SelectableList<Emoji>
    | BreakingChange of BreakingChange * invalidInput: string option // TODO RDE: SelectableList<?> - ? can be `Yes | No` then Disabled means not in the list
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

type Model = {
    CurrentStep: CurrentStep
    CompletedSteps: CompletedStep list

    /// The prefixes available depend on the eventual emoji selected
    AvailablePrefixes: Prefix list

    /// The emojis available depend on the eventual prefix selected
    AvailableEmojis: Emoji list

    SearchMode: SearchMode

    // TODO: to remove once the copy to clipboard is implemented
    PreviousFullCompletion: (Prefix * Emoji * BreakingChange) option

// TODO: Backspace Key Hint Cases
// TODO: Enter Key Hint Cases
// TODO: Emoji Char Hint Optionality
// TODO: BreakingChange Char Hint Optionality
}

type Msg =
    | Enter
    | Down
    | Up
    | InputChanged of input: string
    | ToggleFullTextSearch of isFullText: bool

// TODO: handle Notices
type Notice =
    | NoNotice
    | InvalidInput of input: string
    | AllStepsCompleted of commitMessageTemplate: string

[<RequireQualifiedAccess>]
module CommandChar =
    [<Literal>]
    let Emoji = ":"