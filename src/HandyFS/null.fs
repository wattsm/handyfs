///Contains functions for working with null values
module HandyFS.Null

    open System

    ///Converts a potentially null value to an option
    let asOption value = 
        match value with
        | null -> None
        | _ -> Some value    

    ///Converts a Nullable<T> value to an option
    let asOption2 (value : Nullable<_>) = 
        if value.HasValue then
            Some value.Value
        else    
            None

    