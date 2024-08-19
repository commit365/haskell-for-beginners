## Web Development with Haskell

Haskell provides several powerful frameworks for web development, with **Yesod** and **Servant** being two of the most notable. Both frameworks emphasize type safety and productivity, allowing developers to build robust web applications.

### Yesod

**Yesod** is a high-performance web framework that makes extensive use of Haskell's type system to ensure safety and correctness at compile time. It is designed for building type-safe, RESTful web applications.

#### Key Features of Yesod:

- **Type Safety**: Yesod leverages Haskell's type system to catch errors at compile time, reducing runtime bugs.
- **Asynchronous I/O**: Built-in support for asynchronous operations, allowing for non-blocking I/O.
- **DSLs**: Provides domain-specific languages (DSLs) for routing, templating, and database access, which are all type-checked at compile time.
- **Performance**: Optimized for performance while maintaining a high level of abstraction.

#### Getting Started with Yesod

1. **Install Yesod**: You can create a new Yesod project using the following command:

   ```bash
   yesod init
   ```

2. **Define Your Application**: Create a simple Yesod application:

   ```haskell
   {-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

   import Yesod

   data App = App

   mkYesod "App" [parseRoutes| / HomeR GET |]

   instance Yesod App

   getHomeR :: Handler Html
   getHomeR = defaultLayout [whamlet|Hello, Yesod!|]

   main :: IO ()
   main = warp 3000 App
   ```

3. **Run Your Application**: Compile and run your application:

   ```bash
   stack run
   ```

4. **Access Your App**: Open your browser and navigate to `http://localhost:3000` to see your Yesod application in action.

### Servant

**Servant** is a type-level web framework that focuses on defining web APIs in a type-safe manner. It allows you to describe your API using Haskell types, and it generates the necessary server and client code.

#### Key Features of Servant:

- **Type-Safe APIs**: Define APIs using Haskell types, ensuring correctness at compile time.
- **Modular Design**: Easily compose APIs from smaller components.
- **Automatic Documentation**: Generate API documentation automatically from the type definitions.
- **Flexible**: Supports both RESTful and GraphQL APIs.

#### Getting Started with Servant

1. **Install Servant**: Add Servant to your project dependencies in your `.cabal` file or `stack.yaml`.

2. **Define Your API**: Create a simple API using Servant:

   ```haskell
   {-# LANGUAGE DataKinds #-}
   {-# LANGUAGE TypeOperators #-}
   {-# LANGUAGE OverloadedStrings #-}
   {-# LANGUAGE DeriveGeneric #-}

   import Servant
   import Network.Wai
   import Network.Wai.Handler.Warp
   import GHC.Generics

   type API = "hello" :> Get '[JSON] HelloResponse

   data HelloResponse = HelloResponse { message :: String } deriving (Generic)

   instance ToJSON HelloResponse

   server :: Server API
   server = return $ HelloResponse "Hello, Servant!"

   app :: Application
   app = serve (Proxy :: Proxy API) server

   main :: IO ()
   main = run 3000 app
   ```

3. **Run Your API**: Compile and run your application:

   ```bash
   stack run
   ```

4. **Access Your API**: Open your browser and navigate to `http://localhost:3000/hello` to see the JSON response.

### Summary

- **Yesod**: A powerful web framework focused on type safety and performance, ideal for building complex web applications.
- **Servant**: A type-level web framework that allows you to define APIs in a type-safe manner, suitable for RESTful services.
- **Getting Started**: Both frameworks provide tools and libraries to help you quickly set up and run web applications.

### Practice Exercises

1. Extend the Yesod application to include routes for creating and viewing users.
2. Implement a more complex API using Servant that includes multiple endpoints and data types.
3. Explore middleware options in both frameworks to add logging or authentication.

### Conclusion

Haskell provides robust frameworks like Yesod and Servant for web development, emphasizing type safety and productivity. By following this guide, you can get started with building web applications and APIs in Haskell. Practice these concepts to become proficient in Haskell web development!
