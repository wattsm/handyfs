module HandyFS.Option

    ///Returns a some value or a default for none
    let someOr default' value = 
        match value with
        | None -> default'
        | Some value' -> value'


        

