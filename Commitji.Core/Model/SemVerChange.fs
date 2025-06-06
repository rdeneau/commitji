namespace Commitji.Core.Model

type SemVerChange =
    | Major
    | Minor
    | Patch

[<RequireQualifiedAccess>]
module SemVerChange =
    let (|HasBreakingChange|_|) (breakingChange: BreakingChange) =
        if breakingChange.Selected then Some() else None

    let determine breakingChange prefix =
        match breakingChange, prefix with
        | HasBreakingChange, _ -> Some SemVerChange.Major
        | _, Prefix.Fix -> Some SemVerChange.Minor
        | _, Prefix.Feat -> Some SemVerChange.Patch
        | _ -> None

    let code =
        function
        | Major -> "+1._._"
        | Minor -> "_.+1._"
        | Patch -> "_._.+1"