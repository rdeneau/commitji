namespace Commitji.Core.Model

type BreakingChange = { Selected: bool; Disabled: bool }

[<RequireQualifiedAccess>]
module BreakingChange =
    let determine emoji prefix =
        match emoji, prefix with
        | Emoji.Boom, _ -> { Selected = true; Disabled = true }
        | _, Prefix.Feat
        | _, Prefix.Fix -> { Selected = false; Disabled = false }
        | _ -> { Selected = false; Disabled = true }