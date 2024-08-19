## Lenses and Prisms

Lenses and prisms are powerful abstractions in Haskell that facilitate the manipulation of complex data structures. They allow you to focus on specific parts of data types and provide a way to get, set, and modify these parts in a concise and type-safe manner.

### Lenses

A **lens** is a first-class reference to a subpart of a data structure. It provides a way to access and update a value within a larger structure without having to manually traverse the structure.

#### Key Concepts of Lenses

- **Getters and Setters**: Lenses provide functions to get and set values. A lens can be thought of as a pair of functions:
  - A getter retrieves a value from a structure.
  - A setter updates a value in a structure.

- **Composition**: Lenses can be composed together to create more complex lenses that can traverse deeper into nested structures.

#### Example of a Lens

Here’s a simple example of how to define and use a lens with a record type:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Person = Person { _name :: String, _age :: Int }
makeLenses ''Person

-- Using the lens
main :: IO ()
main = do
    let person = Person "Alice" 30
    print $ person ^. name       -- Get the name
    let updatedPerson = person & age .~ 31  -- Update the age
    print $ updatedPerson
```

In this example:
- `makeLenses` automatically generates lenses for the fields of the `Person` data type.
- The `^.` operator is used to get the value of a lens, and the `&` operator is used to update a value using the lens.

### Prisms

A **prism** is a type of optic that allows you to work with sum types (like `Either` or custom algebraic data types). Prisms are useful for pattern matching and constructing values of a sum type.

#### Key Concepts of Prisms

- **Upcasting and Downcasting**: Prisms provide a way to inject a value into a sum type (upcasting) and to attempt to extract a value from a sum type (downcasting).

- **Pattern Matching**: Prisms can be used to match against specific constructors of a sum type.

#### Example of a Prism

Here’s how to define and use a prism with an `Either` type:

```haskell
import Control.Lens

-- Define a prism for the Left constructor of Either
_left :: Prism' (Either a b) a
_left = prism' Left $ \e -> case e of
    Left a  -> Just a
    Right _ -> Nothing

-- Using the prism
main :: IO ()
main = do
    let value = Left 42 :: Either Int String
    print $ value ^? _left      -- Just 42
    print $ value ^? _right     -- Nothing

    let newValue = review _left 100  -- Create a new Left value
    print newValue
```

In this example:
- The `_left` prism allows you to work with the `Left` constructor of the `Either` type.
- The `^?` operator is used to safely extract a value using the prism, returning `Nothing` if the match fails.
- The `review` function is used to create a new `Left` value from a regular value.

### Summary

- **Lenses**: Provide a way to access and modify parts of a data structure in a type-safe manner. They work well with product types (like records).
- **Prisms**: Allow you to work with sum types and provide a way to inject and extract values from these types. They are useful for pattern matching against specific constructors.

### Practice Exercises

1. Create a more complex data structure (e.g., a nested record) and define lenses for its fields.
2. Implement a prism for a custom algebraic data type with multiple constructors.
3. Use lenses and prisms to manipulate a list of records, updating specific fields based on certain conditions.

### Conclusion

Lenses and prisms are powerful tools in Haskell that enhance the way you work with data structures. By using these abstractions, you can write more concise, readable, and maintainable code. Practice these concepts to become proficient in using lenses and prisms in Haskell!
