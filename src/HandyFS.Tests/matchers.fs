[<AutoOpen>]
module Matchers

    open System
    open NHamcrest

    let Some'<'a when 'a : equality> (x : 'a) = 
        {
            new Matcher<obj> () with

                override this.ToString () =
                    String.Format ("Some ({0})", x)

                override this.Matches value = 

                    let debug = value

                    match value with 
                    | :? ('a option) as opt ->
                        match opt with
                        | Some y -> x = y
                        | _ -> false
                    |  _ -> false
        }

    let None'<'a> = 
        {
            new Matcher<obj> () with

                override this.ToString () = 
                    "None"

                override this.Matches value = 
                    match value with
                    | :? ('a option) as opt -> Option.isNone opt                        
                    | _ -> false
        }

