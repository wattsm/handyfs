module HandyFS.Context

    module Monad = 

        ///Binds two functions, passing the context to the first and the result and context to the second
        let bind f g = 
            fun context -> 
                let result = f context
                in g result context

        ///Infix operator for bind
        let (-->) = 
            bind

        ///Lifts a value to the context monad form
        let return' x = 
            fun _ ->
                x

    ///Gets the context value
    let getContext () = 
        id

    ///Workflow builder for the context monad
    type ContextBuilder () = 

        member this.Bind (expr, rest) =
            Monad.bind expr rest

        member this.Return expr = 
            Monad.return' expr

    ///Syntactic sugar function for the context workflow
    let context = 
        ContextBuilder ()