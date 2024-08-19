## JSON Parsing and Generation in Haskell

**Aeson** is a popular library in Haskell for working with JSON. It provides functions for parsing JSON data into Haskell data types and generating JSON from Haskell values. This guide will cover how to install Aeson, parse JSON, and generate JSON.

### Setting Up Aeson

To use Aeson, you need to include it in your project. If you are using Cabal, add the following line to your `.cabal` file under `build-depends`:

```cabal
build-depends: aeson >= 2.0 && < 3.0
```

If you are using Stack, add `aeson` to your `stack.yaml` file under `extra-deps` or include it in your `package.yaml`.

### Importing Aeson

To use Aeson in your Haskell file, import the necessary modules:

```haskell
import Data.Aeson
import qualified Data.ByteString.Lazy as B
```

### Defining Data Types

To parse JSON into Haskell data types, you need to define the data types that correspond to the JSON structure. You also need to derive the `FromJSON` and `ToJSON` type classes for these types.

#### Example: Defining a Data Type

Let’s define a simple data type for a person:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)

data Person = Person
    { name :: String
    , age  :: Int
    } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person
```

### Parsing JSON

You can parse JSON data from a file or a string using the `decode` function. The JSON data must be in `ByteString` format.

#### Example: Parsing JSON from a File

Here’s how to read JSON data from a file and parse it into a `Person` object:

```haskell
import Data.Aeson
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    -- Read JSON from a file
    jsonData <- B.readFile "person.json"
    
    -- Parse the JSON data
    let person = decode jsonData :: Maybe Person
    
    case person of
        Just p  -> print p
        Nothing -> putStrLn "Failed to parse JSON."
```

### Example JSON File

Here’s an example of what the `person.json` file might look like:

```json
{
    "name": "Alice",
    "age": 30
}
```

### Generating JSON

You can generate JSON from Haskell data types using the `encode` function. This function takes a value of a type that implements the `ToJSON` type class and returns the corresponding JSON in `ByteString` format.

#### Example: Generating JSON

Here’s how to create a `Person` object and generate JSON from it:

```haskell
import Data.Aeson
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    -- Create a Person object
    let person = Person "Bob" 25
    
    -- Generate JSON from the Person object
    let jsonData = encode person
    
    -- Write JSON to a file
    B.writeFile "output.json" jsonData
```

### Example Output

When you run the above code, it will create an `output.json` file with the following content:

```json
{"name":"Bob","age":25}
```

### Summary

- **Aeson**: A library for parsing and generating JSON in Haskell.
- **Data Types**: Define Haskell data types that correspond to the JSON structure and derive `FromJSON` and `ToJSON`.
- **Parsing JSON**: Use `decode` to parse JSON data from a file or string into Haskell data types.
- **Generating JSON**: Use `encode` to convert Haskell data types into JSON format.

### Practice Exercises

1. Extend the `Person` data type to include an address and modify the JSON parsing and generation accordingly.
2. Implement a program that reads a list of people from a JSON file and prints their names and ages.
3. Create a function that takes a list of `Person` objects and generates a JSON array from them.

### Conclusion

Working with JSON in Haskell using the Aeson library is straightforward and efficient. By following the techniques outlined in this guide, you can easily parse and generate JSON data in your Haskell applications. Practice these concepts to become proficient in JSON handling with Aeson!