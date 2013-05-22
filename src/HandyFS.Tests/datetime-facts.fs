module ``DateTime Facts``

    open System
    open FsUnit.Xunit
    open Xunit

    let datetime = DateTime (2013, 4, 17, 13, 50, 0)

    module ``Time Module`` =

        type ``At method`` () =

            [<Fact>] member test.
                ``Returns the given time of the date`` () =
                    datetime.At 9 30 15 |> should equal (DateTime (2013, 4, 17, 9, 30, 15))

        type ``Midnight method`` () =

            [<Fact>] member test.
                ``Returns midnight of the current date`` () =
                    datetime.Midnight () |> should equal (DateTime (2013, 4, 17, 0, 0, 0))

    module ``Date Module`` = 

        type ``GetDaysInMonth month`` () = 

            [<Fact>] member test.
                ``Returns the correct list of dates`` () =  
                    let 
                        expected = 
                            [ 1; 8; 15; 22; 29; ]
                            |> List.map (fun day -> DateTime (2013, 4, day))
                    in
                        Date.getDaysInMonth 2013 4 DayOfWeek.Monday
                        |> should equal expected

    module ``DateTime Extensions`` = 

        type ``Next method`` () =

            [<Fact>] member test.
                ``Returns the correct date when the next occurrence of the reqested day is in current week`` () =
                    datetime.Next (DayOfWeek.Friday) |> should equal (DateTime (2013, 4, 19, 13, 50, 0))

            [<Fact>] member test.
                ``Returns the correct date when next occurrence of the requested day is in next week`` () =
                    datetime.Next (DayOfWeek.Tuesday) |> should equal (DateTime (2013, 4, 23, 13, 50, 0))

        type ``Previous method`` () =

            [<Fact>] member test.
                ``Returns the correct date when the previous occurrence of the requested day is in the current week`` () =
                    datetime.Previous (DayOfWeek.Tuesday) |> should equal (DateTime (2013, 4, 16, 13, 50, 0))

            [<Fact>] member test.
                ``Returns the correct date when the previous occurrence of the requested day is in the previous week`` () =
                    datetime.Previous (DayOfWeek.Friday) |> should equal (DateTime (2013, 4, 12, 13, 50, 0))

        type ``StartOfWeek method`` () =

            let monday = 
                DateTime (2013, 4, 15, 13, 50, 0)

            [<Fact>] member test.
                ``Returns the correct date when the date is a Monday`` () =
                    monday.StartOfWeek () |> should equal monday

            [<Fact>] member test.
                ``Returns the correct date then when date is not a Monday`` () =
                    datetime.StartOfWeek () |> should equal monday

    