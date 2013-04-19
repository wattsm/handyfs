///Contains functions for working with null values
module Null

    open System

    ///Converts a nullable value to an option
    let asOption (value : Nullable<_>) = 
        if value.HasValue then
            Some value.Value
        else    
            None

    ///Converts an option to a definite value, using the type's default when the value is None
    let asDefault<'a> (value : 'a option) = 
        match value with
        | None -> Unchecked.defaultof<'a>
        | Some value' -> value'
        