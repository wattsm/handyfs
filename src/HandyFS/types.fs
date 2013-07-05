module HandyFS.Types

    open System

    ///True if a type is the specified type
    let isType<'a> = 
        ((=) typedefof<'a>)

    ///True if a type is the specified generic type
    let isGenericType<'a> (t : Type) = 
        if t.IsGenericType then
            t.GetGenericTypeDefinition () = typedefof<'a>
        else
            false

    ///True if a type is optional, i.e. takes the form 'a option
    let isOptional =
        isGenericType<Option<_>>

    ///True if a type is unit / ()
    let isUnit : Type -> bool =
        isType<unit>


