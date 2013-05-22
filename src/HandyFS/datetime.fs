///Contains extensions to the standard DateTime type
[<AutoOpen>]
module DateTime

    open System

    ///Contains functions for manipulating times
    [<RequireQualifiedAccess>]
    module Time = 

        ///Sets the time of a given datetime
        let at hour min sec (datetime : DateTime) = 
            DateTime (datetime.Year, datetime.Month, datetime.Day, hour, min, sec)

        ///Sets the given datetime to midnight
        let midnight = 
            at 0 0 0

    ///Contains functions for manipulating dates
    [<RequireQualifiedAccess>]
    module Date = 

        ///Adds a given number of days to a date
        let addDays (num : int) (datetime : DateTime) = 
            datetime.AddDays (float num)

        ///Sets the next day of the week for a given datetime
        let next day (datetime : DateTime) = 
            
            let targetDay = (int day)
            let currentDay = (int datetime.DayOfWeek) 

            let difference =
                match (targetDay - currentDay) with
                | d when d < 0 -> d + 7
                | d -> d              

            datetime.AddDays (float difference)

        ///Sets the next day of the week for a given datetime, excluding the current day
        let exclusiveNext day = 
            addDays 1
            >> next day

        ///Sets the previous day of the week for a given datetime
        let previous day (datetime : DateTime) = 

            let targetDay = (int day)
            let currentDay = (int datetime.DayOfWeek)

            let difference = 
                match (targetDay - currentDay) with
                | d when d > 0 -> d - 7
                | d -> d

            datetime.AddDays (float difference)

        ///Sets the previous day of the week for a given datetime, excluding the current day
        let exclusivePrevious day = 
            addDays -1
            >> previous day

        ///Sets the day of the week to be the Monday of the given date's week
        let startOfWeek (datetime : DateTime) = 
            if (datetime.DayOfWeek = DayOfWeek.Monday) then
                datetime
            else
                datetime
                |> previous DayOfWeek.Monday        

        ///Gets the dates of all given days of the week in a month
        let getDaysInMonth year month dayOfWeek = 

            let rec checkAndProgress (date : DateTime) = 
                if (date.Month <> month) then
                    []
                else
                    date :: (date |> exclusiveNext dayOfWeek |> checkAndProgress)

            DateTime (year, month, 1)
            |> addDays -1 //End of previous month
            |> next dayOfWeek
            |> checkAndProgress
            
        ///Gets the dates of all given days of the week in the current month
        let getDaysInCurrentMonth dayOfWeek = 
            getDaysInMonth DateTime.Now.Year DateTime.Now.Month dayOfWeek

    ///Extension methods for the the DateTime type
    type DateTime with

        ///Returns the current date at the given time
        member this.At hour min sec = 
            this
            |> Time.at hour min sec

        ///Returns midnight of the current date
        member this.Midnight () =
            this 
            |> Time.midnight

        ///Gets the date of the next occurrence of a day of the week
        member this.Next (day : DayOfWeek) = 
            this
            |> Date.next day

        ///Gets the date of the previous occurrence of a day of the week
        member this.Previous (day : DayOfWeek) = 
            this
            |> Date.previous day

        ///Sets the day of the week to be the Monday of the date's week
        member this.StartOfWeek () =
            this
            |> Date.startOfWeek