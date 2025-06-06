module Commitji.Core.Helpers

[<RequireQualifiedAccess>]
module Reflection =
    open Microsoft.FSharp.Reflection

    let getEnumLikeUnionCases<'T> () : 'T list =
        let t = typeof<'T>

        [
            if FSharpType.IsUnion t then
                for c in FSharpType.GetUnionCases t do
                    if c.GetFields().Length = 0 then
                        FSharpValue.MakeUnion(c, [||]) :?> 'T
        ]