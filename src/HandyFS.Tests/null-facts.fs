module ``Null facts`` 

    open System
    open FsUnit.Xunit
    open Xunit
    open Null

    type ``asOption function`` () =

        [<Fact>] member test.
            ``Returns None when value is null`` () =
                let 
                    value = Nullable<int> ()
                in
                    value |> asOption |> ((=) None) |> should be True
        
        [<Fact>] member test.
            ``Returns Some when value is not null`` () =
                let
                    value = Nullable (10)
                in
                    value |> asOption |> should equal (Some 10)

    type ``asDefault function`` () =

        [<Fact>] member test.
            ``Returns the type default when value is None`` () =
                let
                    value : int option = None
                in
                    value |> asDefault |> should equal 0

        [<Fact>] member test.
            ``Returns the wrapped value when the value is Some`` () =
                let
                    value = Some 10
                in
                    value |> asDefault |> should equal 10