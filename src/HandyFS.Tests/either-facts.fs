module ``Either facts``

    open HandyFS.Either
    open HandyFS.Either.Monad
    open FsUnit.Xunit
    open Xunit
    open System

    [<Trait (TraitNames.Module, ModuleNames.State)>]
    module ``eitherApply function`` = 

        let [<Fact>] ``Left function is applied to Left value`` () =
            
            let value = Left "abc"
            let result = eitherApply (fun a -> a + "123") (fun b-> b + "456") value

            result |> should equal "abc123"

        let [<Fact>] ``Right function is applied to Right value`` () =
            
            let value = Right "abc"
            let result = eitherApply (fun a -> a + "123") (fun b-> b + "456") value

            result |> should equal "abc456"
            

    [<Trait (TraitNames.Module, ModuleNames.State)>]
    module ``lefts function`` = 

        let [<Fact>] ``All Lefts are in collection`` () =
            
            let values = [Right "123"; Left "abc"; Right "def";  Right "jkl";Left "ghi";]
            let result = lefts values

            result |> List.same ["abc"; "ghi"] |> should be True
            result |> List.contains "def" |> should be False
            result |> List.contains "jkl" |> should be False
            
        let [<Fact>] ``Empty list returns empty result`` () =
            
            let values = []
            let result = lefts values

            result.Length |> should equal 0

        let [<Fact>] ``Only right list returns empty result`` () =
            
            let values = [Right "123"; Right "def";  Right "jkl";]
            let result = lefts values

            result.Length |> should equal 0

    [<Trait (TraitNames.Module, ModuleNames.State)>]
    module ``rights function`` = 

        let [<Fact>] ``All rights are in collection`` () =
            
            let values = [Right "123"; Left "abc"; Right "def";  Right "jkl";Left "ghi";]
            let result = rights values

            result |> List.same ["123"; "def"; "jkl"] |> should be True
            result |> List.contains "abc" |> should be False
            result |> List.contains "ghi" |> should be False
            
        let [<Fact>] ``Empty list returns empty result`` () =
            
            let values = []
            let result = rights values

            result.Length |> should equal 0

        let [<Fact>] ``Only left list returns empty result`` () =
            
            let values = [Left "123"; Left "def";  Left "jkl";]
            let result = rights values

            result.Length |> should equal 0

    [<Trait (TraitNames.Module, ModuleNames.State)>]
    module ``partitionEithers function`` = 

        let [<Fact>] ``All rights are in collection`` () =
            
            let values = [Right "123"; Left "abc"; Right "def";  Right "jkl";Left "ghi";]
            let result = partitionEithers values

            (fst result) |> List.same ["abc"; "ghi"] |> should be True
            (fst result).Length |> should equal 2

            (snd result) |> List.same ["123"; "def"; "jkl"] |> should be True
            (snd result).Length |> should equal 3

    [<Trait (TraitNames.Module, ModuleNames.Maybe)>]
    module ``bind function`` =

        let dummy (num : Int32) = 
            return' (num * 2)

        let [<Fact>] ``Function is called when value is Right`` () =
            let result = bind (Right 2) dummy 
            result |> should equal (Right 4)

        let [<Fact>] ``Function is not called when value is Left`` () =
            let result = bind (Left 2) dummy 
            (result = (Left 2)) |> should be True

    [<Trait (TraitNames.Module, ModuleNames.Maybe)>]
    module ``Monad laws`` =

        let assertEquality (f : string -> either<'a,'b>) (g : string -> either<'a,'b>) = 

            let fv = f "abc" 
            let gv = g "abc" 

            Assert.Equal (fv, gv)

        let isEven n = n % 2 = 0        
        let incrementEven n = if (isEven n) then Some (n + 1) else None
        let incrementOdd n = if not (isEven n) then Some (n + 1) else None
        let doubleEven n = if (isEven n) then Some (n * 2) else None
        let appendChar str = Right (str + "!")

        let [<Fact>] ``return a >>= f = f a`` () =

            let f n = bind (return' n) appendChar
            let g = appendChar

            assertEquality f g

        let [<Fact>] ``f >>= return = f`` () = 

            let f n = bind (appendChar n) return'
            let g = appendChar

            assertEquality f g

        let [<Fact>] ``f >>= (fun x -> g x >>= h) = (f >>= g) >>= h)`` () = 

            let f n = bind (appendChar n) (fun n' -> bind (appendChar n') appendChar)
            let g n = bind (bind (appendChar n)  appendChar) appendChar

            assertEquality f g  

    [<Trait (TraitNames.Module, ModuleNames.Maybe)>]
    module ``either builder`` =

        let dummy (num : Int32) = 
            Monad.return' (num * 2)

        let [<Fact>] ``bind is invoked when value is Right`` () =
            let result = either {
                let! interim1 = Right 1
                return 2
            }
            result |> should equal (Right 2)

        let [<Fact>] ``bind is not invoked when value is Left`` () =
            let result = either {
                let! interim1 = Left 1
                return 2
            }
            (result = (Left 1)) |> should be True