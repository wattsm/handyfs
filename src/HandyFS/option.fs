module HandyFS.Option

    ///Returns a some value or a default for none
    let someOr default' value = 
        match value with
        | None -> default'
        | Some value' -> value'

    ///Converts an option to a definite value, using the type's default when the value is None
    let asDefault<'a> (value : 'a option) = 
        match value with
        | None -> Unchecked.defaultof<'a>
        | Some value' -> value'


        

