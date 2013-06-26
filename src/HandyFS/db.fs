///Contains functions for working with databases
module HandyFS.DB

    open System
    open System.Data
    open System.Data.Common
    open System.Configuration

    ///Gets the factory for the named provider
    let getFactory : string -> DbProviderFactory = 
        DbProviderFactories.GetFactory

    ///Creates and opens a connection using the given factory
    let createConnection (factory : DbProviderFactory) = 
        factory.CreateConnection ()

    ///Creates a command using the given connection
    let createCommand (connection : DbConnection) = 
        
        let command = 
            connection.CreateCommand ()

        command.Connection <- connection

        command

    ///Sets the command type of a command
    let setType commandType (command : DbCommand) =
        command.CommandType <- commandType
        command    

    ///Sets the command text of a command
    let setText commandText (command : DbCommand) = 
        command.CommandText <- commandText
        command

    ///Describes types of parameter
    type Parameter<'a> = 
        | Input of (string * 'a)
        | Output of string

    ///Sets the name of a parameter
    let setName name (parameter : DbParameter) = 
        parameter.ParameterName <- name
        parameter

    ///Sets the direction of a parameter
    let setDirection direction (parameter : DbParameter) = 
        parameter.Direction <- direction
        parameter

    ///Sets the value of a parameter
    let setValue value (parameter : DbParameter) = 
        
        let value' = 
            match value with
            | None -> box DBNull.Value
            | Some v -> box v

        parameter.Value <- value'
        parameter

    ///Creates a parameter based on the given command
    let createParameter parameter (cmd : DbCommand) = 
        
        let name, direction, value = 
            match parameter with
            | Input (n, v) -> n, ParameterDirection.Input, Some v
            | Output n -> n, ParameterDirection.Output, None

        cmd.CreateParameter ()
        |> setName name
        |> setValue value
        |> setDirection direction

    ///Adds a parameter to a command
    let addParameter param  =
        fun (cmd : DbCommand) ->
            createParameter param cmd
            |> cmd.Parameters.Add
            |> ignore

    ///Opens a connection to a given database
    let openConnection connectionString (connection : DbConnection) = 

        connection.ConnectionString <- connectionString
        connection.Open ()

        connection

    ///Connects to a database using the given factory
    let connectUsingFactory connectionString = 
        createConnection
        >> openConnection connectionString  

    ///Connects to a database using the given connection string
    let connectUsingSettings providerName connectionString = 
        providerName
        |> getFactory
        |> connectUsingFactory connectionString

    ///Gets the settings for a given connection name
    let getConnectSettings (connectionName : string) =
        match ConfigurationManager.ConnectionStrings.[connectionName] with
        | null -> None
        | settings -> Some (settings.ProviderName, settings.ConnectionString)

    ///Connects to a database using a named connection string
    let connectUsingName (connectionName : string) = 
        match (getConnectSettings connectionName) with
        | None -> invalidArg "connectionName" "No connection string with the given name could be found"
        | Some (providerName, connectionString) ->
            connectUsingSettings
            <| providerName
            <| connectionString    

    ///Describes ways of connecting to a database
    type ConnectionMethod = 
        | ByName of String
        | BySettings of String * String

    ///Connects to a database using the given method
    let connect connectionMethod = 
        match connectionMethod with
        | ByName name -> 
            connectUsingName name

        | BySettings (providerName, connectionString) -> 
            connectUsingSettings
            <| providerName
            <| connectionString

    ///Describes types of DB command
    type CommandSettings = 
        | Procedure of String
        | Sql of String

    ///Computational workflow wrapping a DB connection and command
    type CommandWorkflow (connection : DbConnection, closeable, settings) = 

        let command = 

            let commandType, commandText = 
                match settings with
                | Procedure procName -> CommandType.StoredProcedure, procName
                | Sql query -> CommandType.Text, query
            
            connection
            |> createCommand 
            |> setType commandType
            |> setText commandText

        let closeConnection () =
            if closeable then
                if connection.State <> ConnectionState.Closed then
                    connection.Close ()
                    connection.Dispose ()

        let dispose () = 
            command.Dispose ()
            closeConnection ()

        member this.Bind (x, f) = 
            f (x command)

        member this.Return x = 
            x

        member this.ReturnFrom f = 
            f command

        member this.Delay f = 
            f

        member this.Run f = 

            let result = f ()

            closeConnection ()

            result

        override this.Finalize () = 
            dispose ()

        interface IDisposable with
            
            member this.Dispose () = 
                dispose () 
                GC.SuppressFinalize (this)

    ///Creates a command workflow for a stored procedure
    let call procName (connection, disposable) = 
        new CommandWorkflow (connection, disposable, (Procedure procName))

    ///Creates a command workflow for a SQL query
    let query sql (connection, disposable) = 
        new CommandWorkflow (connection, disposable, (Sql sql))

    ///Syntactic sugar function for use with the command workflow
    let usingConnection connection = 
        (connection, false)

    ///Syntactic sugar function for use with the command workflow
    let usingNamedConnection connectionName = 
        (connectUsingName connectionName, true)

    ///Executes a given command as a scalar. For use with the command workflow.
    let execScalar () = 
        fun (cmd : DbCommand) ->
            cmd.ExecuteScalar ()

    ///Executes a scalar and converts the value. For use with the command workflow.
    let execScalarOf convert =
        fun (cmd : DbCommand) ->

            let raw = 
                cmd.ExecuteScalar ()

            let value = 
                if (raw = (box DBNull.Value)) then
                    None
                else
                    Some raw

            convert value

    ///Executes a given command as a non-query. For use with the command workflow.
    let execNonQuery () =
        fun (cmd : DbCommand) ->
            cmd.ExecuteNonQuery ()

    ///Execute s given command as a reader, parsing each row using the given function.
    let execRead getItem =
        fun (cmd : DbCommand) ->
        
            use reader = 
                cmd.ExecuteReader ()

            let data =
                seq {
                    while reader.Read () do
                        yield getItem reader
                }

            List.ofSeq data //Forces evaluation of sequence