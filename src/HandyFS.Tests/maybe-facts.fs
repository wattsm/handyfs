module ``Maybe facts``

    open System
    open FsUnit.Xunit
    open Xunit
    open HandyFS.Maybe.Monad

    [<Trait (TraitNames.Module, ModuleNames.Maybe)>]
    module ``bind function`` =

        let dummy (num : Int32) = 
            Some (num * 2)

        let [<Fact>] ``Function is called when value is not None`` () =
            bind (Some 2) dummy |> Option.get |> should equal 4

        let [<Fact>] ``Function is not called when value is None`` () =
            bind None dummy |> Option.isNone |> should be True            

    [<Trait (TraitNames.Module, ModuleNames.Maybe)>]
    module ``Monad laws`` =

        let assertEquality (f : Int32 -> Int32 option) (g : Int32 -> Int32 option) = 

            let fv = f 2 |> Option.get
            let gv = g 2 |> Option.get

            Assert.Equal (fv, gv)

        let isEven n = n % 2 = 0        
        let incrementEven n = if (isEven n) then Some (n + 1) else None
        let incrementOdd n = if not (isEven n) then Some (n + 1) else None
        let doubleEven n = if (isEven n) then Some (n * 2) else None

        let [<Fact>] ``return a >>= f = f a`` () =

            let f n = return' n --> incrementEven
            let g = incrementEven

            assertEquality f g

        let [<Fact>] ``f >>= return = f`` () = 

            let f n = incrementEven n --> return'
            let g = incrementEven

            assertEquality f g

        let [<Fact>] ``f >>= (fun x -> g x >>= h) = (f >>= g) >>= h)`` () = 

            let f n = incrementEven n --> (fun n' -> incrementOdd n' --> doubleEven)
            let g n = incrementEven n --> incrementOdd --> doubleEven

            assertEquality f g