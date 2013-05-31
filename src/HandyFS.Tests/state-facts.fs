module ``State facts``

    open HandyFS.State.Monad
    open FsUnit.Xunit
    open Xunit
    open System

    [<Trait (TraitNames.Module, ModuleNames.State)>]
    module ``bind function`` = 

        let [<Fact>] ``Current state state value is passed to first function`` () =
            
            let first state = ((), state)
            let second _ state = state

            let bound = 
                bind first second

            bound 1 |> should equal 1


        let [<Fact>] ``Result of first function and new state value are passed to second function`` () =
            
            let first state = ("ABC123", state + 2)
            let second arg state = (arg, state)

            let bound = 
                bind first second

            bound 1 |> should equal ("ABC123", 3)

    [<Trait (TraitNames.Module, ModuleNames.State)>]
    module ``Monad laws`` =

        let incValue value = fun state -> (value + 1, state)
        let incState _ = fun state -> ((), state + 1)
        let add value = fun state -> ((), state + value)

        let assertEquality (f : Int32 -> (unit * Int32)) (g : Int32 -> (unit * Int32)) = 

            let fstate = f 2 |> snd
            let gstate = g 2 |> snd

            Assert.Equal (fstate, gstate)

        let [<Fact>] ``return a >>= f = f a`` () =
            
            let f = return' 10 --> incState
            let g = incState 10

            assertEquality f g

        let [<Fact>] ``f >>= return = f`` () =
            
            let f = incState 10 --> return'
            let g = incState 10

            assertEquality f g

        let [<Fact>] ``f >>= (fun x -> g x >>= h) = (f >>= g) >>= h`` () =
            
            let f = incValue 5 --> (fun n -> add n --> incState)
            let g = (incValue 5 --> add) --> incState

            assertEquality f g

