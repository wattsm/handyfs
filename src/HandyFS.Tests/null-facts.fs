module ``Null facts`` 

    open System
    open FsUnit.Xunit
    open Xunit
    open HandyFS.Null

    module ``asOption function`` =

        let [<Fact>] ``Returns None when value is null`` () =
            let 
                value = Nullable<int> ()
            in
                value |> asOption |> ((=) None) |> should be True
        
        let [<Fact>] ``Returns Some when value is not null`` () =
            let
                value = Nullable (10)
            in
                value |> asOption |> should equal (Some 10)

    module ``asDefault function`` =

        let [<Fact>] ``Returns the type default when value is None`` () =
            let
                value : int option = None
            in
                value |> asDefault |> should equal 0

        let [<Fact>] ``Returns the wrapped value when the value is Some`` () =
            let
                value = Some 10
            in
                value |> asDefault |> should equal 10