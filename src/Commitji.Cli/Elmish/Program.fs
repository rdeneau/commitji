// Adapted from https://github.com/elmish/elmish/blob/v4.x/src/program.fs
namespace Commitji.Cli.Elmish

/// Dispatch - feed new message into the processing loop
type Dispatch<'msg> = 'msg -> unit

/// Program type captures various aspects of program behavior
type Program<'arg, 'model, 'msg, 'view> = private {
    init: 'arg -> 'model
    update: 'msg -> 'model -> 'model
    view: 'model -> Dispatch<'msg> -> 'view
    setState: 'model -> Dispatch<'msg> -> unit
    termination: ('msg -> bool) * ('model -> unit)
}

/// Program module - functions to manipulate program instances
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Program =
    let mkProgram (init: 'arg -> 'model) (update: 'msg -> 'model -> 'model) (view: 'model -> Dispatch<'msg> -> 'view) = {
        init = init
        update = update
        view = view
        setState = fun model -> view model >> ignore
        termination = (fun _ -> false), ignore
    }

    /// Exit criteria and the handler, overrides existing.
    let withTermination (predicate: 'msg -> bool) (terminate: 'model -> unit) (program: Program<'arg, 'model, 'msg, 'view>) = { program with termination = predicate, terminate }

    /// Map existing criteria and the handler.
    let mapTermination map (program: Program<'arg, 'model, 'msg, 'view>) = { program with termination = map program.termination }

    /// Function to render the view with the latest state
    let withSetState (setState: 'model -> Dispatch<'msg> -> unit) (program: Program<'arg, 'model, 'msg, 'view>) = { program with setState = setState }

    /// Return the function to render the state
    let setState (program: Program<'arg, 'model, 'msg, 'view>) = program.setState

    /// Return the view function
    let view (program: Program<'arg, 'model, 'msg, 'view>) = program.view

    /// Return the init function
    let init (program: Program<'arg, 'model, 'msg, 'view>) = program.init

    /// Return the update function
    let update (program: Program<'arg, 'model, 'msg, 'view>) = program.update

    /// Map the program type
    let map mapInit mapUpdate mapView mapSetState mapTermination (program: Program<'arg, 'model, 'msg, 'view>) = {
        init = mapInit program.init
        update = mapUpdate program.update
        view = mapView program.view
        setState = mapSetState program.setState
        termination = mapTermination program.termination
    }

    /// <summary>
    /// Start the single-threaded dispatch loop.
    /// </summary>
    /// <param name="arg">argument to pass to the <c>init</c> function.</param>
    /// <param name="program">program created with <c>Program.mkProgram</c>.</param>
    let runWith (arg: 'arg) (program: Program<'arg, 'model, 'msg, 'view>) =
        let model = program.init arg
        let shouldTerminate, terminate = program.termination
        let msgBuffer = RingBuffer(size = 10)

        let mutable state = model
        let mutable reentered = false
        let mutable terminated = false

        let rec dispatch msg =
            if not terminated then
                msgBuffer.Push msg

                if not reentered then
                    reentered <- true
                    processMsgs ()
                    reentered <- false

        and processMsgs () =
            let mutable nextMsg = msgBuffer.Pop()

            while not terminated && Option.isSome nextMsg do
                let msg = nextMsg.Value

                if shouldTerminate msg then
                    terminate state
                    terminated <- true
                else
                    let model' = program.update msg state
                    program.setState model' dispatch
                    state <- model'

                nextMsg <- msgBuffer.Pop()

        reentered <- true
        program.setState model dispatch
        processMsgs ()
        reentered <- false

    /// Start the dispatch loop for an `init` function taking no parameter.
    let run (program: Program<unit, 'model, 'msg, 'view>) = runWith () program