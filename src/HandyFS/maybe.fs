(**
    Maybe monad

    Used to chain functions together where each function returns either Some x or None. When Some x
    is returned the next function along is called with x as an argument. When None is returned the next function
    along is not called, and None is returned.

    Example:

        let connect : unit -> Connection option
        let getCommand : String -> Connection -> Command option
        let execute : Command -> Result option

        Using workflow:

            // String -> Result option
            let execSql sql = 
                maybe {

                    let! connection = connect ()
                    let! command = getCommand sql connection 

                    return (execute command)
                }

        Using monad functions:

            // String -> Result option
            let execSql sql = 
                return ()
                --> connect
                --> getCommand sql
                --> (execute >> return')

**)

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