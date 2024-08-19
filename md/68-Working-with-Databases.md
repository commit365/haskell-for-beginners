## Working with Databases in Haskell

Haskell provides several libraries to interact with databases, and one of the most popular is `persistent`. This library allows you to define data models, perform CRUD (Create, Read, Update, Delete) operations, and work with various databases, including PostgreSQL.

### Setting Up Persistent and PostgreSQL

1. **Install Dependencies**: You need to add the necessary libraries to your project. If you are using Cabal, add the following to your `.cabal` file under `build-depends`:

   ```cabal
   build-depends:
       persistent >= 2.0 && < 3.0,
       persistent-postgresql >= 2.0 && < 3.0,
       postgresql-simple >= 0.6 && < 1.0
   ```

   If you are using Stack, add these dependencies to your `stack.yaml` file or `package.yaml`.

2. **Install PostgreSQL**: Make sure you have PostgreSQL installed and running on your machine. You can download it from [the official PostgreSQL website](https://www.postgresql.org/download/).

### Importing Necessary Modules

To work with `persistent` and PostgreSQL, import the required modules:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad (forM_)
```

### Defining Data Models

You can define your data models using Template Haskell. Here’s an example of a simple `User` model:

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    age Int
    deriving Show
|]
```

### Connecting to the Database

You need to establish a connection to your PostgreSQL database. You can do this using the `connectPostgreSQL` function.

```haskell
-- Connection string format: "host=localhost dbname=mydb user=myuser password=mypassword"
connStr :: String
connStr = "host=localhost dbname=mydb user=myuser password=mypassword"

runDb :: SqlPersistT IO a -> IO a
runDb query = do
    pool <- createPostgresqlPool connStr 10  -- Connection pool with 10 connections
    runSqlPool query pool
```

### Performing CRUD Operations

Now that you have your database set up, you can perform CRUD operations.

#### Create

To insert a new user into the database:

```haskell
insertUser :: String -> Int -> SqlPersistT IO (Key User)
insertUser name age = insert $ User name age
```

#### Read

To retrieve users from the database:

```haskell
getUsers :: SqlPersistT IO [Entity User]
getUsers = selectList [] []
```

#### Update

To update a user’s information:

```haskell
updateUser :: Key User -> String -> Int -> SqlPersistT IO ()
updateUser userId newName newAge = update userId [UserName =. newName, UserAge =. newAge]
```

#### Delete

To delete a user from the database:

```haskell
deleteUser :: Key User -> SqlPersistT IO ()
deleteUser userId = delete userId
```

### Putting It All Together

Here’s a complete example that demonstrates connecting to the database, creating a user, retrieving users, updating a user, and deleting a user:

```haskell
main :: IO ()
main = runDb $ do
    runMigration migrateAll  -- Run migrations to create tables

    -- Create a new user
    userId <- insertUser "Alice" 30
    liftIO $ putStrLn $ "Inserted user with ID: " ++ show userId

    -- Read users
    users <- getUsers
    liftIO $ putStrLn "Users in the database:"
    forM_ users $ \(Entity _ user) -> liftIO $ print user

    -- Update the user
    updateUser userId "Alice Smith" 31
    liftIO $ putStrLn "Updated user."

    -- Read users again
    updatedUsers <- getUsers
    liftIO $ putStrLn "Updated users in the database:"
    forM_ updatedUsers $ \(Entity _ user) -> liftIO $ print user

    -- Delete the user
    deleteUser userId
    liftIO $ putStrLn "Deleted user."
```

### Example Database Setup

Before running the above code, ensure you have a PostgreSQL database created. You can create a database using the following SQL command in the PostgreSQL shell:

```sql
CREATE DATABASE mydb;
```

### Summary

- **Persistent**: A library for working with databases in Haskell, allowing you to define data models and perform CRUD operations.
- **PostgreSQL**: Use the `persistent-postgresql` library to connect and interact with PostgreSQL databases.
- **CRUD Operations**: You can create, read, update, and delete records using the provided functions.

### Practice Exercises

1. Extend the `User` model to include an email field and modify the CRUD operations accordingly.
2. Implement a function to retrieve users older than a certain age.
3. Create a command-line interface (CLI) that allows users to interact with the database (e.g., adding, updating, and deleting users).

### Conclusion

Working with databases in Haskell using the `persistent` library provides a powerful and type-safe way to manage data. By following the techniques outlined in this guide, you can effectively interact with PostgreSQL and perform various database operations. Practice these concepts to become proficient in database management with Haskell!