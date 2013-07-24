module HandyFS.Types

    open System
    open System.Reflection
    open System.Collections.Generic

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

    ///Create a generic type with the given type arguments
    let makeGenericType (baseType : Type) (types : Type list) = 

        if (not baseType.IsGenericTypeDefinition) then
            invalidArg "baseType" "The type specified was not a generic type definition."

        baseType.MakeGenericType (
            types
            |> List.toArray
        )    

    ///Gets a list of the properties of a type
    let getProperties (type' : Type) = 
        type'.GetProperties ()
        |> Array.toList

    ///Gets a list of the writeable properties of a type
    let getWriteableProperties : Type -> PropertyInfo list = 
        getProperties 
        >> List.filter (fun p -> p.CanWrite)

    ///Gets a list of the readable properties of a type
    let getReadableProperties : Type -> PropertyInfo list = 
        getProperties
        >> List.filter (fun p -> p.CanRead)

    ///Gets the type of an object
    let getType instance = 
        instance.GetType ()

    ///Casts an object to a given type
    let castAs<'a> (instance : obj) = 
        instance :?> 'a

    ///Creates a list of the given type 
    let makeListOf itemType (items : obj list)  = 

        let listType = 
            makeGenericType 
            <| typedefof<Microsoft.FSharp.Collections.List<_>> 
            <| [ itemType; ]

        let add = 

            let cons = 
                listType.GetMethod ("Cons")
            
            fun item list ->
                cons.Invoke (null, [| item; list; |])                

        let instance = 

            let empty = 
                listType.GetProperty ("Empty")

            empty.GetValue (null)

        instance
        |> List.foldBack add items

    ///True if a type is an F# list
    let isList = 
        isGenericType<Microsoft.FSharp.Collections.List<_>>

    ///Creates a sequence of a given type
    let makeSeqOf itemType (items : obj list) = 

        let seqType = 
            makeGenericType
            <| typedefof<List<_>>
            <| [ itemType; ]

        let add = 

            let addMethod = 
                seqType.GetMethod ("Add")           
            
            fun seq item ->
                addMethod.Invoke (seq, [| item; |]) |> ignore
                seq
                
        let instance = 
            Activator.CreateInstance seqType

        items
        |> List.fold add instance            

    ///True if a type is an F# sequence (IEnumerable<T>)
    let isSeq = 
        isGenericType<IEnumerable<_>>

    ///Gets a value of None for a give type 
    let makeNone wrappedType = 

        let optionalType = 
            makeGenericType typedefof<Option<_>> [ wrappedType ]

        let noneProperty = 
            optionalType.GetProperty ("None")

        noneProperty.GetValue (null)

    ///Gets a value of Some for a given type
    let makeSome wrappedType value =

        let optionalType = 
            makeGenericType typedefof<Option<_>> [ wrappedType ]

        let someMethod = 
            optionalType.GetMethod ("Some")

        someMethod.Invoke (null, [| value |])

    ///Describes the result of mapping data to a constructor
    type ConstructorMapping = {
        Constructor : ConstructorInfo;
        Arguments : (String * (obj option)) list;
        Suitability : double;
    }

    ///Contains funtions used to find constructors
    [<RequireQualifiedAccess>]
    module Constructor = 

        ///Finds a constructor by its parameter names and types
        let findByArguments (type' : Type) (parameters : (String * Type) list) = 
        
            let isParameterMatch (p : ParameterInfo) = 
                parameters
                |> List.exists (fun (parameterName, parameterType) ->
                        String.same p.Name parameterName
                            && p.ParameterType = parameterType
                    )

            let isConstructorMatch (c : ConstructorInfo) =
                c.GetParameters ()
                |> Array.forall isParameterMatch

            type'.GetConstructors ()
            |> Array.tryFind isConstructorMatch

        ///True if a given constructor is the default
        let isDefault (constructorInfo : ConstructorInfo) = 
            constructorInfo.GetParameters ()
            |> Array.length
            |> ((=) 0)        

        ///Maps a list of arguments to a constructor
        let mapByArgumentNames (arguments : (String * obj) list) (constructorInfo : ConstructorInfo) = 

            let parameters = 
                constructorInfo.GetParameters ()
            
            let arguments' =    
                parameters
                |> Array.Parallel.map (fun parameterInfo ->
                        
                        let value = 
                            arguments
                            |> List.tryPick (fun (name, value') -> 
                                    if (String.same parameterInfo.Name name) then   
                                        Some value'
                                    else
                                        None
                                )

                        (parameterInfo.Name, value)
                    )
                |> Array.toList

            let suitability =  

                let paramCount = 
                    parameters
                    |> Array.length
                    |> double

                let argCount = 
                    arguments'
                    |> List.filter (snd >> Option.isSome)
                    |> List.length
                    |> double

                (argCount / paramCount) * (double 100)

            {
                Constructor = constructorInfo;
                Arguments = arguments';
                Suitability = suitability;
            }

        ///Gets mappings of all non-default constructors 
        let getMappingsByArgumentNames (type' : Type) arguments = 
            type'.GetConstructors ()
            |> Array.filter (not << isDefault)
            |> Array.Parallel.map (mapByArgumentNames arguments)
            |> Array.toList

        ///Gets the public, non-default constructor which best matches the given argument list by name
        let getBestMatchByArgumentNames type' arguments = 
            match (getMappingsByArgumentNames type' arguments) with
            | [] -> None
            | mappings ->
                mappings
                |> List.maxBy (fun mapping -> mapping.Suitability)
                |> Some            

        ///Gets the default constructor for a type
        let getDefault (type' : Type) = 
            type'.GetConstructor (Type.EmptyTypes)

        ///Finds the default constructor for a type
        let findDefault : Type -> ConstructorInfo option = 
            getDefault >> Null.asOption

    ///Contains functions used to summarise types
    module Summaries = 

        ///Union used to describe groups that types can generally be assigned to
        type TypeGroup = 
            | Unit'
            | Optional of Type
            | Definite

        ///Gets the group to which a type belongs
        let getTypeGroup t = 
            if (isUnit t) then
                Unit'
            else if (isOptional t) then
            
                t.GetGenericArguments ()
                |> Array.head
                |> Optional

            else
                Definite
        
        ///Record describing the basic categories of non-optional typs
        type TypeCategory = 
            | Simple
            | Complex
            | Collection of TypeSummary

        ///Record summarising a type
        and TypeSummary = {
            Type : Type;
            BaseType : Type;
            IsOptional : Boolean;
            Category : TypeCategory;
        }
        with

            ///Summary of the unit type
            static member Unit = 
                {
                    Type = typeof<Unit>;
                    BaseType = typeof<Unit>;
                    IsOptional = false;
                    Category = Simple;
                }

        ///True if a summary is of a collection
        let isCollection summary = 
            match summary with
            | Collection _ -> true
            | _ -> false

        ///True if a summary is of a given type
        let isCollectionOf (itemType : Type) summary = 
            match summary with
            | Collection itemSummary -> itemSummary.Type = itemType
            | _ -> false

        ///Tyrue if a summary is of a simple type
        let isSimple summary = 
            summary = Simple

        ///True if a summary is of a complex type
        let isComplex summary = 
            summary = Complex

        ///Gets the category of a given type
        let rec getTypeCategory (type' : Type) = 
            if (type'.IsValueType || type' = typeof<String>) then
                Simple
            else           

                let itemType = 

                    let enumerableType =
                        if (isGenericType<IEnumerable<_>> type') then
                            Some type'
                        else
                            type'.GetInterfaces ()
                            |> Array.tryFind isGenericType<IEnumerable<_>>

                    match enumerableType with
                    | Some enumerableType' -> 
                        enumerableType'.GetGenericArguments ()
                        |> Array.head
                        |> Some
                    | _ -> None
                        
                match itemType with
                | Some itemType' -> 
                    itemType'
                    |> getTypeSummary
                    |> Collection
                | _ ->
                    Complex

        ///Summarises a type
        and getTypeSummary (type' : Type) = 
            match (getTypeGroup type') with
            | Unit' -> 
                TypeSummary.Unit;

            | Definite -> 
                {
                    Type = type';
                    BaseType = type';
                    IsOptional = false;
                    Category = (getTypeCategory type');
                }

            | Optional baseType ->
                {
                    Type = type';
                    BaseType = baseType;
                    IsOptional = true;
                    Category = (getTypeCategory baseType);
                }