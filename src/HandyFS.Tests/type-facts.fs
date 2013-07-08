module ``Types facts``

    open System
    open System.Collections.Generic
    open FsUnit.Xunit
    open Xunit
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

    [<Trait (TraitNames.Module, ModuleNames.Types)>]
    module ``makeGenericType function`` =

        let [<Fact>] ``Creates correct generic type`` () = 
            makeGenericType typedefof<Option<_>> [ typeof<String>; ] |> should equal typeof<Option<String>>

        let [<Fact>] ``Raises an InvalidOperationException if the type specified is not a generic type definition`` () =
            (fun () -> makeGenericType typeof<String> [] |> ignore) |> should throw typeof<InvalidOperationException>
            
            
            

