module ``Types facts``

    open System
    open System.Collections.Generic
    open System.Reflection
    open FsUnit.Xunit
    open Xunit
    open Xunit.Extensions
    open HandyFS.Types

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``isType function`` = 

        let [<Fact>] ``Returns true when the types match`` () = 
            typeof<String> |> isType<String> |> should be True

        let [<Fact>] ``Returns true when the generic types match`` () =
            typedefof<Option<_>> |> isType<Option<_>> |> should be True

        let [<Fact>] ``Returns false when the types do not match`` () =
            typeof<Int32> |> isType<String> |> should be False

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``isGenericType function`` =

        let [<Fact>] ``Returns true when the types match`` () =
            typeof<IList<String>> |> isGenericType<IList<_>> |> should be True

        let [<Fact>] ``Returns false when the types do not match`` () =
            typeof<IList<String>> |> isGenericType<ICollection<_>> |> should be False

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``isUnit function`` =

        let [<Fact>] ``Returns true when type is unit`` () =
            typeof<unit> |> isUnit |> should be True

        let [<Fact>] ``Returns false when type is not unit`` () =
            typeof<String> |> isUnit |> should be False

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``isOptional function`` =

        let [<Fact>] ``Returns true when type is optional`` () =
            typeof<String option> |> isOptional |> should be True

        let [<Fact>] ``Returns false when type is not optional`` () =
            typeof<String> |> isOptional |> should be False           
    

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``makeGenericType function`` =

        let [<Fact>] ``Creates correct generic type`` () = 
            makeGenericType typedefof<Option<_>> [ typeof<String>; ] |> should equal typeof<Option<String>>

        let [<Fact>] ``Raises an InvalidOperationException if the type specified is not a generic type definition`` () =
            (fun () -> makeGenericType typeof<String> [] |> ignore) |> should throw typeof<InvalidOperationException>         

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``makeListOf function`` = 

        let [<Fact>] ``Creates a list of the correct type`` () =
            (makeListOf typeof<Int32> []) :? int list |> should be True            

        let [<Fact>] ``Creates a list with the correct contents`` () = 
            
            let list = 
                makeListOf typeof<Int32> [ box 1; box 2; box 3; ] :?> int list

            list
            |> should equal [ 1; 2; 3; ]

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``makeSeqOf function`` =

        let [<Fact>] ``Creates a seq of the correct type`` () =
            (makeSeqOf typeof<Int32> []) :? int seq |> should be True

        let [<Fact>] ``Creates a sequence with the correct contents`` () =
            
            let seq = 
                makeSeqOf typeof<Int32> [ box 1; box 2; box 3; ] :?> int seq

            seq
            |> List.ofSeq
            |> should equal [ 1; 2; 3; ]

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``makeNone function`` = 

        let [<Fact>] ``Returns correct value`` () =
            
            let isCorrect =     
                match (makeNone typeof<Int32>) with
                | :? (int option) as value -> value |> Option.isNone                    
                | _ -> false

            isCorrect |> should be True

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``makeSome function`` = 

        let [<Fact>] ``Returns correct value`` () =
            
            let isCorrect = 
                match (makeSome typeof<Int32> 10) with
                | :? (int option) as value ->
                    match value with 
                    | Some x -> x = 10
                    | _ -> false
                | _ -> false

            isCorrect |> should be True

    module ``Summaries facts`` = 

        open HandyFS.Types.Summaries

        [<Trait (TraitNames.Module, ModuleNames.TypesSummaries)>]
        module ``getTypeGroup function`` = 

            let [<Fact>] ``Returns Unit' when type is Unit`` () =
                typeof<Unit> |> getTypeGroup |> should equal Unit'

            let [<Fact>] ``Returns Optional when type is an option`` () =
            
                let isOption result = 
                    match result with
                    | Optional _ -> true
                    | _ -> false

                typeof<string option> |> getTypeGroup |> isOption |> should be True

            let [<Fact>] ``Returns correct underlying type when type is an option`` () =

                let isOptionalString result = 
                    match result with
                    | Optional t -> t = typeof<string>
                    | _  -> false

                typeof<string option> |> getTypeGroup |> isOptionalString |> should be True

            let [<Fact>] ``Returns Definite when the type is not an option`` () =
                typeof<string> |> getTypeGroup |> should equal Definite

        [<Trait (TraitNames.Module, ModuleNames.TypesSummaries)>]
        module ``getTypeCategory function`` = 

            let [<Fact>] ``Value types are categorised as Simple`` () =
                typeof<Int32>
                |> getTypeCategory
                |> should equal Simple

            let [<Fact>] ``String is categorised as Simple`` () =
                typeof<String>
                |> getTypeCategory
                |> should equal Simple

            let [<Fact>] ``T list is categorised as Collection`` () =
                typeof<Int32 list>
                |> getTypeCategory
                |> isCollection
                |> should be True

            let [<Fact>] ``Collection item type is set correctly for T list`` () =
                typeof<Int32 list>
                |> getTypeCategory
                |> isCollectionOf typeof<Int32>
                |> should be True

            let [<Fact>] ``T seq is categorised as Collection`` () =
                typeof<Int32 seq>
                |> getTypeCategory
                |> isCollection
                |> should be True

            let [<Fact>] ``Collection item type is set correctly for T seq`` () =
                typeof<Int32 seq>
                |> getTypeCategory
                |> isCollectionOf typeof<Int32>
                |> should be True

            let [<Fact>] ``T array is categorised as Collection`` () =
                typeof<Int32 array>
                |> getTypeCategory
                |> isCollection 
                |> should be True

            let [<Fact>] ``Collection item type is set correctly for T array`` () =
                typeof<Int32 array>
                |> getTypeCategory
                |> isCollectionOf typeof<Int32>
                |> should be True

            let [<Fact>] ``IEnumerable<T> is categorised as Collection`` () =
                typeof<IEnumerable<Int32>>
                |> getTypeCategory
                |> isCollection
                |> should be True

            let [<Fact>] ``Collection item type is set correctly for IEnumerable<T>`` () =
                typeof<IEnumerable<Int32>>
                |> getTypeCategory
                |> isCollectionOf typeof<Int32>
                |> should be True

            let [<Fact>] ``Non-Simple, non-Collection types are categorised as Complex`` () =   
                typeof<obj>
                |> getTypeCategory
                |> isComplex
                |> should be True

        [<Trait (TraitNames.Module, ModuleNames.TypesSummaries)>]
        module ``getTypeSummary function`` = 

            let [<Fact>] ``IsOptional is set to true for optional types`` () = 

                let isOptional summary = 
                    summary.IsOptional

                typeof<Int32 option>
                |> getTypeSummary
                |> isOptional
                |> should be True

            let [<Fact>] ``BaseType is set correctly for optional types`` () =
                
                let isCorrectBaseType summary = 
                    summary.BaseType = typeof<Int32>

                typeof<Int32 option>
                |> getTypeSummary
                |> isCorrectBaseType
                |> should be True

            let [<Fact>] ``BaseType is set correctly for non-optional types`` () =
                
                let isCorrectBaseType summary = 
                    summary.BaseType = typeof<Int32>

                typeof<Int32>
                |> getTypeSummary
                |> isCorrectBaseType
                |> should be True

    module ``Constructor facts`` = 

        [<Trait (TraitNames.Module, ModuleNames.TypesConstructor)>]
        module ``findByArguments function``  =

            type Dummy (arg1 : String, arg2 : Int32) = 
                class end
              
            let [<Fact>] ``Finds constructor if one exists`` () =

                let parameters = 
                    [ ("arg1", typeof<String>); ("arg2", typeof<Int32>); ]

                let isCorrectConstructor (c : ConstructorInfo option) = 
                    match c with
                    | None -> false
                    | Some info ->
                        info.GetParameters ()
                        |> Array.map (fun p -> (p.Name, p.ParameterType))
                        |> Array.toList
                        |> List.same parameters

                parameters
                |> Constructor.findByArguments typeof<Dummy>
                |> isCorrectConstructor 
                |> should be True        

            let [<Fact>] ``Nothing is returned if no matching constructor exists`` () =
                [ ("arg1", typeof<DateTime>); ]
                |> Constructor.findByArguments typeof<Dummy>
                |> Option.isNone 
                |> should be True

        [<Trait (TraitNames.Module, ModuleNames.TypesConstructor)>]
        module ``findDefault function`` =

            type Dummy1 () =
                class end

            type Dummy2 (arg : String) = 
                class end

            let [<Fact>] ``Finds constructor if one exists`` () =

                let isDefault result = 
                    match result with
                    | Some (info : ConstructorInfo) -> info.GetParameters () |> Array.isEmpty
                    | _ -> false

                typeof<Dummy1> |> Constructor.findDefault |> isDefault |> should be True

            let [<Fact>] ``Nothing is returned if no default constructor exists`` () =
                typeof<Dummy2> |> Constructor.findDefault |> Option.isNone |> should be True

        [<Trait (TraitNames.Module, ModuleNames.TypesConstructor)>]
        module ``mapByArgumentNames function`` = 

            let getArg name mapping = 
                mapping.Arguments
                |> List.pick (fun (name', value) ->
                        if (String.same name name') then
                            Some value
                        else
                            None
                    )       
                    
            let getSuitability mapping = 
                mapping.Suitability

            type Dummy (a : string, b : string) = 
                class end

            let target = 
                typeof<Dummy>.GetConstructors ()
                |> Array.head

            let [<Fact>] ``Matched arguments are paired with the matched value`` () =
                let value = (box "Hello") in                    
                    target
                    |> Constructor.mapByArgumentNames [ ("a", value); ]
                    |> getArg "a"
                    |> should be (Some' value)

            let [<Fact>] ``Unmatched arguments are paired with nothing`` () =                
                target
                |> Constructor.mapByArgumentNames [ ("a", box "Hello"); ]
                |> getArg "b"
                |> Option.isNone
                |> should be True

            let [<Fact>] ``Suitability rating is the percentage of arugments matched`` () =
                target
                |> Constructor.mapByArgumentNames [ ("a", box "Hello"); ]
                |> getSuitability
                |> should equal (double 50)

        [<Trait (TraitNames.Module, ModuleNames.TypesConstructor)>]
        module ``getBestMatchByArgumentNames function`` = 

            type Private private () = 
                class end

            type Dummy (a : string, b : string, c : string) = 

                new (x : string, y : string) = 
                    Dummy (x, y, String.Empty)

            let [<Fact>] ``Nothing is returned if the type has no public constructors`` () =
                [ ("a", box "Hello"); ]                
                |> Constructor.getBestMatchByArgumentNames typeof<Private>
                |> Option.isNone
                |> should be True

            let [<Fact>] ``The constructor with the highest suitability rating is returned`` () =

                let isCorrect result = 
                    match result with
                    | Some mapping -> mapping.Constructor.GetParameters () |> Array.length |> ((=) 2)
                    | _ -> false

                [ ("x", box "Hello"); ]
                |> Constructor.getBestMatchByArgumentNames typeof<Dummy>
                |> isCorrect
                |> should be True            