(**
    Context monad

    Chains functions together, threading a context through them. The first function accepts just the context, with subsequent
    functions accepting the result of the previous function and the context.

    Example:

        let settings = [ ("greeting", "Hello, {0}"); ]

        Using workflow:

            let sayHello (name : String) =
                context {

                    let! settings = getContext ()

                    let greeting = 
                        settings 
                        |> List.pick (fun (key, value) -> 
                                match key with
                                | "greeting" -> Some value
                                | _ -> None
                            )

                    return String.Format (greeting, name)
                }

        Using monad functions:

            let sayHello name =

                let getGreeting (name : String) (settings : (String * String) list) = 
                    
                    let greeting = 
                        settings
                        |> List.pick (fun (key, value) ->
                                match key with
                                | "greeting" -> Some value
                                | _ -> None
                            )

                    String.Format (greeting, name)

                return' name
                --> getGreeting
**)

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