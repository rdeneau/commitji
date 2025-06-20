namespace Commitji.Core.Model

open Commitji.Core.Helpers
open Commitji.Core.Model.Search

[<RequireQualifiedAccess>]
type BreakingChange =
    | Yes
    | No

    member this.AsBool =
        match this with
        | Yes -> true
        | No -> false

    member this.AsString =
        match this with
        | Yes -> "Yes"
        | No -> "No"

    member this.Toggle() =
        match this with
        | Yes -> No
        | No -> Yes

[<RequireQualifiedAccess>]
module BreakingChange =
    let All = Reflection.getEnumLikeUnionCases<BreakingChange> ()

    let ofBool b = // ↩
        if b then BreakingChange.Yes else BreakingChange.No

    let ofString =
        function
        | String.Eq "Y"
        | String.Eq "Yes" -> Some BreakingChange.Yes
        | String.Eq "N"
        | String.Eq "No" -> Some BreakingChange.No
        | _ -> None

    let private selectableList (breakingChanges: BreakingChange list) =
        breakingChanges
        |> SearchableList.init (fun _ x -> [ SearchSegment.SearchableByStart(SegmentId.Code, x.AsString) ])
        |> SelectableList.init

    /// Determine the available breaking changes matching on the given emoji and prefix.
    let determine emoji prefix : SelectableList<BreakingChange> =
        match emoji, prefix with
        | Emoji.Boom, _ -> selectableList [ BreakingChange.Yes ]
        | _, Prefix.Feat
        | _, Prefix.Fix -> selectableList [ BreakingChange.Yes; BreakingChange.No ] |> SelectableList.select BreakingChange.No
        | _ -> selectableList [ BreakingChange.No ]