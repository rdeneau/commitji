namespace Commitji.Core.Model

open Commitji.Core

/// <summary>
/// Git commit prefix, based on Angular conventional commits (*) with some changes:
/// <br/> - Removed `style` prefix: use `refactor` instead.
/// <br/> - Removed `build` and `ci` prefixes: use `chore` instead.
/// <br/> - Added `revert` prefix for reverting commits.
/// <br/> - Added `wip` prefix for work in progress commits.
/// <para>
/// (*) https://github.com/angular/angular/blob/main/contributing-docs/commit-message-guidelines.md
/// </para>
/// </summary>
type Prefix =
    // Mainly used prefixes
    | Feat
    | Fix
    | Refactor
    | Test

    // Other prefixes
    | Chore
    | Docs
    | Perf
    | Revert
    | Wip

[<RequireQualifiedAccess>]
module Prefix =
    let All = Helpers.Reflection.getEnumLikeUnionCases<Prefix> ()

    let props prefix =
        let code, hint =
            match prefix with
            | Feat -> "feat", "A new feature"
            | Fix -> "fix", "A bug fix"
            | Refactor -> "refactor", "A code change that does not alter the functionality"
            | Test -> "test", "Adding missing tests"
            | Chore -> "chore", "Any other changes finalized: config, build, ci, dependencies..."
            | Docs -> "docs", "Documentation only changes"
            | Perf -> "perf", "A code change that improves performance"
            | Revert -> "revert", "Reverts a previous commit"
            | Wip -> "wip", "Work in progress, not yet finalized"

        {| Code = code; Hint = hint |}