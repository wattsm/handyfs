module HandyFS.Maybe

    module Monad = 

        ///Calls f with the given value so long as it is not none
        let bind value f = 
            match value with
            | Some x -> f x
            | None -> None

        ///Infix operator for bind
        let (-->) = 
            bind

        ///Lifts a value to the default form for the maybe monad
        let return' x = 
            Some x

    ///Workflow builder for the maybe monad
    type MaybeBuilder () = 

        member this.Bind (expr, rest) = 
            Monad.bind expr rest

        member this.Return x = 
            Monad.return' x

    ///Workflow sugar for the maybe monad
    let maybe = 
        MaybeBuilder ()