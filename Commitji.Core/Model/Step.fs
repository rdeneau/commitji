﻿namespace Commitji.Core.Model

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
    | Confirmation

[<RequireQualifiedAccess>]
module StepName =
    let All = Reflection.getEnumLikeUnionCases<StepName> ()