namespace Commitji.Cli.Elmish

[<RequireQualifiedAccess>]
module private Array =
    let doubleSize index (items: 't array) = [| // ↩
        yield! items |> Seq.skip index
        yield! items |> Seq.take index
        yield! Array.zeroCreate items.Length
    |]

[<Struct>]
type internal RingState<'t> =
    | Writable of items: 't array * index: int
    | ReadWritable of items: 't array * writeIndex: int * readIndex: int

type internal RingBuffer<'t>(size) =
    let mutable state: RingState<'t> = Writable(Array.zeroCreate (max size 10), 0)

    member _.Pop() =
        match state with
        | Writable _ -> // ↩
            None

        | ReadWritable(items, writeIndex, readIndex) ->
            state <-
                let readIndex' = (readIndex + 1) % items.Length

                match readIndex' = writeIndex with
                | true -> Writable(items, writeIndex)
                | false -> ReadWritable(items, writeIndex, readIndex')

            Some items[readIndex]

    member _.Push(item: 't) =
        match state with
        | Writable(items, index) ->
            items[index] <- item
            let writeIndex = (index + 1) % items.Length
            state <- ReadWritable(items, writeIndex, index)

        | ReadWritable(items, writeIndex, readIndex) ->
            items[writeIndex] <- item

            state <-
                let writeIndex' = (writeIndex + 1) % items.Length

                match writeIndex' = readIndex with
                | true -> ReadWritable(items |> Array.doubleSize readIndex, items.Length, 0)
                | false -> ReadWritable(items, writeIndex', readIndex)