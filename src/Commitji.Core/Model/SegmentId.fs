namespace Commitji.Core.Model

open Commitji.Core.Helpers

[<RequireQualifiedAccess>]
type SegmentId =
    | Number
    | Code
    | Hint

[<RequireQualifiedAccess>]
module SegmentId =
    let All = Reflection.getEnumLikeUnionCases<SegmentId> ()