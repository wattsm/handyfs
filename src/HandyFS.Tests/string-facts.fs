module ``String facts``

    open FsUnit.Xunit
    open Xunit

    module ``Same function`` =

        let [<Fact>] ``Returns true if strings are identical`` () =
            String.same "String" "String" |> should be True

        let [<Fact>] ``Returns true if strings are identical, ignoring case`` () =
            String.same "String" "string" |> should be True

        let [<Fact>] ``Returns false if strings are different`` () =
            String.same "ABC" "DEF" |> should be False