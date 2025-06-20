namespace Commitji.Core.Model

open Commitji.Core.Model.Search

/// Represents a searchable list where the item with the given index is the one selected in the list.
type SelectableList<'t> = { Items: SearchableList<'t>; Index: int }

type SelectableList =
    static member init items = // ↩
        { Items = items; Index = 0 }

    static member select item (list: SelectableList<'t>) = // ↩
        { list with Index = list.Items |> List.findIndex (fun x -> x.Item = item) }