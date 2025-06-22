namespace Commitji.Core.Model

open Commitji.Core.Helpers

[<RequireQualifiedAccess>]
type StepType =
    | Selection of nbChoices: int
    | Confirmation

[<RequireQualifiedAccess>]
type StepName =
    | Prefix
    | Emoji
    | BreakingChange
    | SemVerChange
    | Confirmation

[<RequireQualifiedAccess>]
module StepName =
    let All = Reflection.getEnumLikeUnionCases<StepName> ()

type StepStatus =
    | Completed of value: string
    | Current
    | Pending
