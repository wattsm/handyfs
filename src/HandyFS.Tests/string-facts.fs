module ``String facts``

    open System
    open FsUnit.Xunit
    open Xunit
    open Xunit.Extensions

    [<Trait (TraitNames.Module, ModuleNames.String)>]
    module ``Same function`` =

        let [<Fact>] ``Returns true if strings are identical`` () =
            String.same "String" "String" |> should be True

        let [<Fact>] ``Returns true if strings are identical, ignoring case`` () =
            String.same "String" "string" |> should be True

        let [<Fact>] ``Returns false if strings are different`` () =
            String.same "ABC" "DEF" |> should be False

    [<Trait (TraitNames.Module, ModuleNames.String)>]
    module ``split function`` = 

        [<Theory>]
        [<InlineData ("")>]
        [<InlineData ("  ")>]
        [<InlineData (null : String)>]
        let ``Null, empty or whitespace string returns an empty list`` str =
            String.split str "," |> List.isEmpty |> should be True

        let [<Fact>] ``Empty entries are removed`` () =
            String.split "Hello,,World" "," |> should equal [ "Hello"; "World"; ]

        let [<Fact>] ``Returns correct value when string does not contain delimiter`` () =
            String.split "Hello|World" "," |> should equal [ "Hello|World"; ]

        let [<Fact>] ``Returns correct value when string does contain delimiter`` () =
            String.split "Hello,World" "," |> should equal [ "Hello"; "World"; ]