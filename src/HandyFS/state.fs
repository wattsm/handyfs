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
        snd

    ///Sets a new state value
    let setState state = 
        fun _ _ ->
            ((), state)

    ///Workflow builder for the state monad
    type StateBuilder () = 

        member this.Bind (expr, rest) = 
            Monad.bind expr rest

        member this.Return expr = 
            Monad.return' expr

    ///Syntactic sugar function for the state workflow
    let state = 
        StateBuilder ()
            
