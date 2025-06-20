module Commitji.Core.Helpers

open System

[<RequireQualifiedAccess>]
module Map =
    let valuesAsList (map: Map<'k, 'v>) = [ for KeyValue(_, value) in map -> value ]

[<RequireQualifiedAccess>]
module Option =
    let ofBool b =
        if b then Some() else None

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

[<RequireQualifiedAccess>]
module String =
    let emptyIfNull (s: string) =
        if isNull s then String.Empty else s

    /// String equality case-insensitive.
    let (|Eq|_|) (other: string) (s: string) =
        String.Equals(s, other, StringComparison.OrdinalIgnoreCase) |> Option.ofBool

    let (|Int|_|) (s: string) =
        match Int32.TryParse s with
        | true, i -> Some i
        | _ -> None

    let (|IsEmpty|IsNotEmpty|) (s: string) =
        if s.Length = 0 then IsEmpty else IsNotEmpty

    let allIndexesOf (value: string) (comparison: StringComparison) (s: string) =
        if String.IsNullOrEmpty(value) then
            invalidArg (nameof value) "The search string must not be empty"

        let rec loop acc start =
            match s.IndexOf(value, start, comparison) with
            | -1 -> List.rev acc
            | idx  -> loop (idx :: acc) (idx + value.Length)

        loop [] 0