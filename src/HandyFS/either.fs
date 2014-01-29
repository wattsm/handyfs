(**
    Either monad

    Used to chain functions together where each function returns one of two types of values.
    When Right x is returned then the next function in the chain is invoked. When Left x is returned
    then the computation chain ends and Left x is returned

    Example:
        
        let connect : unit -> either<Connection, string>
        let getCommand : String -> Connection -> either<Command, string>
        let execute : Command -> Result
        let execute' : Command -> either<Result,string>

        Using workflow:

            Using execute: 

                // String -> Result option
                let execSql sql = 
                    either {

                        let! connection = connect ()
                        let! command = getCommand sql connection 

                        return (execute command)
                    }

            Or using execute':

                // String -> either<Result,string>
                let execSql sql = 
                    maybe {

                        let! connection = connect ()
                        let! command = getCommand sql connection

                        return! (execute' command)
                    }

        Using monad functions:

            Using execute:

                // String -> either<Result,string>
                let execSql sql = 
                    return ()
                    --> connect
                    --> getCommand sql
                    --> (execute >> return')

            Or using execute':

                // String -> either<Result,string>
                let execSql sql = 
                    return () 
                    --> connect
                    --> getCommand sql
                    --> execute'
**)

module HandyFS.Either
    
    [<StructuralEqualityAttribute; NoComparisonAttribute>]
    type either<'a, 'b> = 
        Left of 'a | Right of 'b
       
    let eitherApply (a : 'a -> 'c) (b : 'b -> 'c) (e : either<'a,'b>) = 
        match e with 
        | Left e' -> a e'
        | Right e' -> b e'

    let lefts eithers = 
        List.choose (function
            | Left x -> Some x
            | _ -> None
        ) eithers

    let rights eithers = 
        List.choose (function
            | Right x -> Some x
            | _ -> None
        ) eithers

    let partitionEithers lst = (lefts lst, rights lst) 

    module Monad = 
        
        let return' value = Right value

        let bind f g = 
            match f with 
            | Right f' -> g f'
            | l -> l

        ///Infix operator for bind
        let (-->) = 
            bind

    type EitherBuilder () = 
        member this.Bind (expr, rest) = 
            Monad.bind expr rest

        member this.Return expr = 
            Monad.return' expr
    
    let either = EitherBuilder()

