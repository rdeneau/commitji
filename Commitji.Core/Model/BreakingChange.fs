namespace Commitji.Core.Model

open Commitji.Core.Helpers
open Commitji.Core.Model.Search

[<RequireQualifiedAccess>]
type BreakingChange =
    | Yes
    | No

    member this.Code =
        match this with
        | Yes -> "Yes"
        | No -> "No"

    member this.Selected =
        match this with
        | Yes -> true
        | No -> false

    member this.Toggle() =
        match this with
        | Yes -> No
        | No -> Yes

[<RequireQualifiedAccess>]
module BreakingChange =
    let All = Reflection.getEnumLikeUnionCases<BreakingChange> ()

    let private selectableList (breakingChanges: BreakingChange list) =
        breakingChanges
        |> SearchableList.init (fun _ x -> [ SearchSegment.SearchableByStart(SegmentId.Code, x.Code) ])
        |> SelectableList.init

    /// Determine the available breaking changes matching on the given emoji and prefix.
    let searchable emoji prefix : SelectableList<BreakingChange> =
        match emoji, prefix with
        | Emoji.Boom, _ -> selectableList [ BreakingChange.Yes ]
        | _, Prefix.Feat
        | _, Prefix.Fix -> selectableList [ BreakingChange.Yes; BreakingChange.No ] |> SelectableList.select BreakingChange.No
        | _ -> selectableList [ BreakingChange.No ]