module ``Types facts``

    open System
    open System.Collections.Generic
    open FsUnit.Xunit
    open Xunit
    open HandyFS.Types

    [<Trait (TraitNames.Module, ModuleNames.Option)>]
    module ``isType function`` = 

        let [<Fact>] ``Returns true when the types match`` () = 
            typedefof<String> |> isType<String> |> should be True

        let [<Fact>] ``Returns false when the types do not match`` () =
            typedefof<Int32> |> isType<String> |> should be False

    [<Trait (TraitNames.Module, ModuleNames.Option)>]
    module ``isGenericType function`` =

        let [<Fact>] ``Returns true when the types match`` () =
            typedefof<IList<String>> |> isGenericType<IList<_>> |> should be True

        let [<Fact>] ``Returns false when the types do not match`` () =
            typedefof<IList<String>> |> isGenericType<ICollection<_>> |> should be False

    [<Trait (TraitNames.Module, ModuleNames.Option)>]
    module ``isUnit function`` =

        let [<Fact>] ``Returns true when type is unit`` () =
            typedefof<unit> |> isUnit |> should be True

        let [<Fact>] ``Returns false when type is not unit`` () =
            typedefof<String> |> isUnit |> should be False

    [<Trait (TraitNames.Module, ModuleNames.Option)>]
    module ``isOptional function`` =

        let [<Fact>] ``Returns true when type is optional`` () =
            typedefof<String option> |> isOptional |> should be True

        let [<Fact>] ``Returns false when type is not optional`` () =
            typedefof<String> |> isOptional |> should be False
            
            

