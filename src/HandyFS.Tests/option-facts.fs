module ``Option facts`` 

    open System
    open FsUnit.Xunit
    open Xunit
    open HandyFS.Option

    [<Trait (TraitNames.Module, ModuleNames.Option)>]
    module ``someOr function`` = 

        let [<Fact>] ``Returns the wrapped value when the value is Some`` () =
            let
                value = Some 10
            in
                value |> someOr 11 |> should equal 10

        let [<Fact>] ``Returns the default value when the wrapped value is None`` () =
            let 
                value = None
            in
                value |> someOr 11 |> should equal 11
            