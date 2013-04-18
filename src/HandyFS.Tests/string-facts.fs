module ``String facts``

    open FsUnit.Xunit
    open Xunit

    type ``Same function`` () =

        [<Fact>] member test.
            ``Returns true if strings are identical`` () =
                String.same "String" "String" |> should be True

        [<Fact>] member test.
            ``Returns true if strings are identical, ignoring case`` () =
                String.same "String" "string" |> should be True

        [<Fact>] member test.
            ``Returns false if strings are different`` () =
                String.same "ABC" "DEF" |> should be False