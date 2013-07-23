module ``Null facts`` 

    open System
    open FsUnit.Xunit
    open Xunit
    open HandyFS.Null

    [<Trait (TraitNames.Module, ModuleNames.Null)>]
    module ``asOption2 function`` =

        let [<Fact>] ``Returns None when value is null`` () =
            let 
                value = Nullable<int> ()
            in
                value |> asOption2 |> Option.isNone |> should be True
        
        let [<Fact>] ``Returns Some when value is not null`` () =
            let
                value = Nullable (10)
            in
                value |> asOption2 |> should equal (Some 10)

    [<Trait (TraitNames.Module, ModuleNames.Null)>]
    module ``asOption function`` = 

        let [<Fact>] ``Returns None when the value is null`` () =
            let 
                value : obj = null
            in
                value |> asOption |> Option.isNone |> should be True

        let [<Fact>] ``Returns Some when value is not null`` () =
            let 
                value = new Object ()
            in
                value |> asOption |> should equal (Some value)
