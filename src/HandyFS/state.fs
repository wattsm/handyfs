(**
    State monad

    Chains functions together, threading a cumulative state through them. The first function accepts just the state and returns
    a tuple of its result and the new state. Subsequent functions accept the result of the previous function plus the updated state.

    Example:

        let add n = fun result state -> (result + n, state)

        Using workflow:

            (See monad function example for commentary.)

            // Int32 -> (unit * Int32)
            let add10 = 
                state {

                    let! value = getState ()
                    let! result = add 10 value
                    do! setState result

                }

        Using monad functions:

            // Int32 -> (unit * Int32)
            let add10 =         // Commentary for add10 5
                return' ()      // Initialises the monad. Data has form (result, state), so here it will be ((), 5)
                --> getState    // Loads the state into the 'result' part of the tuple, i.e. (5, 5)
                --> add 10      // Invokes add function which a value of 10 which results in (15, 5)
                --> setState    // Loads the current value into the state and clears the current value resulting in ((), 15)
**)

module HandyFS.State 

    module Monad = 

        ///Binds two function, calling the first with the current state and passing the result and new state to the second
        let bind f g = 
            fun state ->
                let result, state' = f state
                in g result state'

        ///Infix operator for bind
        let (-->) =  bind

        ///Lifts a value into the state monad format
        let return' value =
            fun state ->
                (value, state)

    ///Gets the current value of the state                    
    let getState () =
        fun state ->
            (state, state)

    ///Sets a new state value
    let setState state = 
        fun _ ->
            ((), state)

    ///Workflow builder for the state monad
    type StateBuilder () = 

        member this.Bind (expr, rest) = 
            Monad.bind expr rest

        member this.Return expr = 
            Monad.return' expr

    ///Workflow builder for use with asynchronous functions
    type AsyncStateBuilder () =

        member this.Bind (expr, rest) = 
            async {
                return 
                    Monad.bind
                    <| (fun state -> expr state |> Async.RunSynchronously)
                    <| (fun value state -> rest value state |> Async.RunSynchronously)
            }

        member this.Return expr = 
            async {
                return (Monad.return' expr)
            }

    ///Syntactic sugar function for the state workflow
    let state = 
        StateBuilder ()

    ///Syntactic sugar function for the async state workflow
    let asyncState = 
        AsyncStateBuilder ()

