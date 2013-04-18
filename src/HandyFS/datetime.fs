///Contains extensions to the standard DateTime type
[<AutoOpen>]
module DateTime

    open System

    type DateTime with

        ///Returns midnight of the current date
        member this.Midnight () =
            DateTime (this.Year, this.Month, this.Day, 0, 0, 0)

        ///Gets the date of the next occurrence of a day of the week
        member this.Next (day : DayOfWeek) = 

            let targetDay = (int day)
            let currentDay = (int this.DayOfWeek)

            let difference =
                match (targetDay - currentDay) with
                | d when d < 0 -> d + 7
                | d -> d              

            this.AddDays (float difference)

        ///Gets the date of the previous occurrence of a day of the week
        member this.Previous (day : DayOfWeek) = 

            let targetDay = (int day)
            let currentDay = (int this.DayOfWeek)

            let difference = 
                match (targetDay - currentDay) with
                | d when d > 0 -> d - 7
                | d -> d

            this.AddDays (float difference)