## Building a RESTful API in Haskell

Creating a RESTful API in Haskell can be achieved using various frameworks, but one of the most popular is **Servant**. Servant allows you to define your API types and automatically generate the necessary handlers. In this guide, we will walk through the steps to build a simple RESTful API using Servant.

### Step 1: Setting Up Your Project

1. **Create a New Project**: If you are using Stack, you can create a new project with:

   ```bash
   stack new my-api
   cd my-api
   ```

2. **Add Dependencies**: Open your `package.yaml` file or `.cabal` file and add the following dependencies:

   ```yaml
   dependencies:
     - servant
     - servant-server
     - http-types
     - aeson
     - text
     - warp
   ```

3. **Install Dependencies**: Run the following command to install the dependencies:

   ```bash
   stack build
   ```

### Step 2: Define Your API

In Haskell, you define your API using a type-level DSL provided by Servant. Here’s an example of a simple API for managing a list of users.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Servant
import Network.Wai
import Network.Wai.Handler.Warp

-- Define a User data type
data User = User
    { userId   :: Int
    , userName :: String
    } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

-- Define the API type
type API = "users" :> Get '[JSON] [User]
      :<|> "users" :> Capture "id" Int :> Get '[JSON] User
      :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User

-- Define a sample user list
users :: [User]
users =
    [ User 1 "Alice"
    , User 2 "Bob"
    ]

-- Define the API handlers
getUsers :: Handler [User]
getUsers = return users

getUserById :: Int -> Handler User
getUserById uid = case filter ((== uid) . userId) users of
    [user] -> return user
    _      -> throwError err404

createUser :: User -> Handler User
createUser user = return user  -- In a real app, you would save this to a database

-- Combine handlers into a server
server :: Server API
server = getUsers :<|> getUserById :<|> createUser

-- Define the main function to run the server
main :: IO ()
main = run 8080 (serve (Proxy :: Proxy API) server)
```

### Step 3: Running the API

1. **Compile and Run**: Compile and run your Haskell application:

   ```bash
   stack run
   ```

2. **Access the API**: Open your browser or use a tool like `curl` or Postman to test your API.

   - **Get All Users**:

     ```bash
     curl http://localhost:8080/users
     ```

     Output:
     ```json
     [{"userId":1,"userName":"Alice"},{"userId":2,"userName":"Bob"}]
     ```

   - **Get User by ID**:

     ```bash
     curl http://localhost:8080/users/1
     ```

     Output:
     ```json
     {"userId":1,"userName":"Alice"}
     ```

   - **Create a New User**:

     You can use a JSON body to create a new user. For example:

     ```bash
     curl -X POST http://localhost:8080/users -H "Content-Type: application/json" -d '{"userId":3,"userName":"Charlie"}'
     ```

     Output:
     ```json
     {"userId":3,"userName":"Charlie"}
     ```

### Conclusion

You have successfully built a simple RESTful API in Haskell using the Servant framework. This API allows you to retrieve a list of users, get a user by ID, and create a new user. 

### Further Reading

- **Servant Documentation**: Explore the [Servant documentation](https://haskell-servant.readthedocs.io/en/stable/) for more advanced features and best practices.
- **Haskell Web Development**: Check out resources on Haskell web development to learn about other frameworks and tools.

### Practice Exercises

1. Extend the API to include PUT and DELETE methods for updating and deleting users.
2. Integrate a database (e.g., PostgreSQL) to persist user data instead of using a static list.
3. Implement authentication for your API using JWT or another method.

By following this guide, you can create a robust RESTful API in Haskell and explore further enhancements and features!