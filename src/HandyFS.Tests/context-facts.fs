module ``Context facts``

    open System
    open FsUnit.Xunit
    open Xunit
    open HandyFS.Context.Monad

    [<Trait (TraitNames.Module, ModuleNames.Context)>]
    module ``bind function`` = 

        let [<Fact>] ``Context is passed to first function`` () =
            
            let first context = context
            let second value _ = value

            let bound = 
                bind first second

            bound 2 |> should equal 2

        let [<Fact>] ``Result of first function and context are passed to second function`` () =
            
            let first context = context + 1
            let second value context = (value, context)

            let bound =     
                bind first second

            bound 2 |> should equal (3, 2)

    [<Trait (TraitNames.Module, ModuleNames.Context)>]
    module ``Monad laws`` = 

        let add x = fun context -> x + context
        let sub x = fun context -> x - context
        let multi x = fun context -> x * context

        let assertEquality (f : Int32 -> Int32) (g : Int32 -> Int32) = 

            let fv = f 10
            let gv = g 10

            Assert.Equal (fv, gv)

        let [<Fact>] ``return a >>= f = f a`` () =
            
            let f = return' 5 --> add
            let g = add 5

            assertEquality f g

        let [<Fact>] ``f >>= return = f`` () =
            
            let f = add 5 --> return'
            let g = add 5

            assertEquality f g

        let [<Fact>] ``f >>= (fun x -> g x >>= h) = (f >>= g) >>= h`` () =
            
            let f = add 5 --> (fun n -> sub n --> multi)
            let g = (add 5 --> sub) --> multi

            assertEquality f g
