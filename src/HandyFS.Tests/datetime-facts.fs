module ``DateTime Facts``

    open System
    open FsUnit.Xunit
    open Xunit

    let datetime = DateTime (2013, 4, 17, 13, 50, 0)

    module ``Time Module`` =

        [<Trait (TraitNames.Module, ModuleNames.DateTime)>]
        module ``At method`` =

            let [<Fact>] ``Returns the given time of the date`` () =
                datetime.At 9 30 15 |> should equal (DateTime (2013, 4, 17, 9, 30, 15))

        [<Trait (TraitNames.Module, ModuleNames.DateTime)>]
        module ``Midnight method`` =

            let [<Fact>] ``Returns midnight of the current date`` () =
                datetime.Midnight () |> should equal (DateTime (2013, 4, 17, 0, 0, 0))

    module ``Date Module`` = 

        [<Trait (TraitNames.Module, ModuleNames.DateTime)>]
        module ``WeekdaysInMonth month`` = 

            let [<Fact>] ``Returns the correct list of dates`` () =  
                let 
                    expected = 
                        [ 1; 8; 15; 22; 29; ]
                        |> List.map (fun day -> DateTime (2013, 4, day))
                in
                    Date.weekdaysInMonth 2013 4 DayOfWeek.Monday
                    |> should equal expected

    module ``DateTime Extensions`` = 

        [<Trait (TraitNames.Module, ModuleNames.DateTime)>]
        module ``Next method`` =

            let [<Fact>] ``Returns the correct date when the next occurrence of the reqested day is in current week`` () =
                datetime.Next (DayOfWeek.Friday) |> should equal (DateTime (2013, 4, 19, 13, 50, 0))

            let [<Fact>] ``Returns the correct date when next occurrence of the requested day is in next week`` () =
                datetime.Next (DayOfWeek.Tuesday) |> should equal (DateTime (2013, 4, 23, 13, 50, 0))

        [<Trait (TraitNames.Module, ModuleNames.DateTime)>]
        module ``Previous method`` =

            let [<Fact>] ``Returns the correct date when the previous occurrence of the requested day is in the current week`` () =
                datetime.Previous (DayOfWeek.Tuesday) |> should equal (DateTime (2013, 4, 16, 13, 50, 0))

            let [<Fact>] ``Returns the correct date when the previous occurrence of the requested day is in the previous week`` () =
                datetime.Previous (DayOfWeek.Friday) |> should equal (DateTime (2013, 4, 12, 13, 50, 0))

        [<Trait (TraitNames.Module, ModuleNames.DateTime)>]
        module ``StartOfWeek method`` =

            let monday = 
                DateTime (2013, 4, 15, 13, 50, 0)

            let [<Fact>] ``Returns the correct date when the date is a Monday`` () =
                monday.StartOfWeek () |> should equal monday

            let [<Fact>] ``Returns the correct date then when date is not a Monday`` () =
                datetime.StartOfWeek () |> should equal monday

        [<Trait (TraitNames.Module, ModuleNames.DateTime)>]
        module ``StartOfMonth method`` =

            let [<Fact>] ``Returns the first of the month at the same time`` () =
                datetime.StartOfMonth () |> should equal (DateTime (2013, 4, 1, 13, 50, 0))

        [<Trait (TraitNames.Module, ModuleNames.DateTime)>]
        module ``EndOfMonth method`` =

            let [<Fact>] ``Returns the last day of the month at the same time`` () =
                datetime.EndOfMonth () |> should equal (DateTime (2013, 4, 30, 13, 50, 0))

    