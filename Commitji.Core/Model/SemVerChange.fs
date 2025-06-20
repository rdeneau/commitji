namespace Commitji.Core.Model

type SemVerChange =
    | Major
    | Minor
    | Patch

    member this.Code =
        match this with
        | Major -> "Major"
        | Minor -> "Minor"
        | Patch -> "Patch"

[<RequireQualifiedAccess>]
module SemVerChange =
    let determine breakingChange prefix =
        match breakingChange, prefix with
        | BreakingChange.Yes, _ -> Some SemVerChange.Major
        | BreakingChange.No, Prefix.Fix -> Some SemVerChange.Minor
        | BreakingChange.No, Prefix.Feat -> Some SemVerChange.Patch
        | BreakingChange.No, _ -> None

    // TODO: to move in the UI with nice markups...
    let code =
        function
        | Major -> "+1._._"
        | Minor -> "_.+1._"
        | Patch -> "_._.+1"