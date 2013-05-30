module ``Collections facts``

    open FsUnit.Xunit
    open Xunit

    module ``List module`` =

        let list = [ 1; 2; 3; 4; 5; ]

        module ``contains function`` =

            let [<Fact>] ``Returns true if item is present in list`` () =
                list |> List.contains 1 |> should be True

            let [<Fact>] ``Returns false if item is not present in list`` () =
                list |> List.contains 7 |> should be False
    
        module ``same function`` =

            let [<Fact>] ``Returns true if lists are identical`` () =
                list |> List.same list |> should be True

            let [<Fact>] ``Returns true if lists contain same items, regardless of order`` () =
                list |> List.same [ 5; 4; 3; 2; 1; ] |> should be True

            let [<Fact>] ``Returns false if lists are of different lengths`` () =
                list |> List.same [ 1; 2; 3; ] |> should be False

            let [<Fact>] ``Returns false if lists contain different items`` () =
                list |> List.same [ 6; 7; 8; 9; 10; 11; ] |> should be False

        module ``intersect function`` =

            let [<Fact>] ``Returns a list containing items that the two lists have in common`` () =
                list |> List.intersect [ 1; 2; 8; ] |> should equal [ 1; 2; ]

            let [<Fact>] ``Returns the empty list if lists have no items in common`` () =
                list |>  List.intersect [ 8; 9; ] |> List.isEmpty |> should be True

        module ``toNvc function`` =

            let [<Fact>] ``Returns collection containing correct items`` () =
                    
                let nvc = 
                    [ ("key-1", "value-1"); ("key-2", "value-2"); ]
                    |> List.toNvc

                Assert.Equal (2, nvc.Count)
                Assert.Equal<string> ("value-1", nvc.["key-1"])
                Assert.Equal<string> ("value-2", nvc.["key-2"])

            let [<Fact>] ``Returns an empty collection if list is empty`` () =
                    
                let nvc = 
                    []
                    |> List.toNvc

                Assert.Equal (0, nvc.Count)

    module ``Enumerable module`` =        

        open System.Collections

        module ``toList function`` = 
    
            let collection = 
                
                let data = ArrayList ()
                data.Add (1) |> ignore
                data.Add (2) |> ignore
                data.Add (3) |> ignore

                data

            let [<Fact>]  ``Returns a list of the collection items`` () =
                collection |> Enumerable.toList<int> |> should equal [ 1; 2; 3; ]

            let [<Fact>] ``Returns an empty list if the collection contains no items`` () =
                ArrayList () |> Enumerable.toList<int> |> List.isEmpty |> should be True

    module ``NameValueCollection module`` = 

        open System.Collections.Specialized

        module ``toList function`` =

            let collection = 

                let data = NameValueCollection ()
                data.["Key.1"] <- "Value.1"
                data.["Key.2"] <- "Value.2"
                data.["Key.3"] <- "Value.3"

                data

            let [<Fact>] ``Returns a list of the collection items as key value pairs`` () =
                    collection |> NameValueCollection.toList |> should equal [ ("Key.1","Value.1"); ("Key.2","Value.2"); ("Key.3","Value.3"); ]

            let [<Fact>] ``Returns an empty list if the collection contains no items`` () =
                    NameValueCollection () |> NameValueCollection.toList |> List.isEmpty |> should be True