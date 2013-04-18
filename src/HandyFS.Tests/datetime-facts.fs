module ``DateTime Facts``

    open System
    open FsUnit.Xunit
    open Xunit

    let datetime = DateTime (2013, 4, 17, 13, 50, 0)

    type ``Midnight method`` () =

        [<Fact>] member test.
            ``Returns midnight of the date`` () =
                datetime.Midnight () |> should equal (DateTime (2013, 4, 17, 0, 0, 0))

    type ``Next method`` () =

        [<Fact>] member test.
            ``Returns the correct date when day is in current week`` () =
                datetime.Next (DayOfWeek.Friday) |> should equal (DateTime (2013, 4, 19, 13, 50, 0))

        [<Fact>] member test.
            ``Returns the correct date when day is in next week`` () =
                datetime.Next (DayOfWeek.Tuesday) |> should equal (DateTime (2013, 4, 23, 13, 50, 0))

    type ``Previous method`` () =

        [<Fact>] member test.
            ``Returns the correct date when the day is in the current week`` () =
                datetime.Previous (DayOfWeek.Tuesday) |> should equal (DateTime (2013, 4, 16, 13, 50, 0))

        [<Fact>] member test.
            ``Returns the correct date when the day is in the previous week`` () =
                datetime.Previous (DayOfWeek.Friday) |> should equal (DateTime (2013, 4, 12, 13, 50, 0))