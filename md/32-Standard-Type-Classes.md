## Standard Type Classes: Eq, Ord, Show, Read

Haskell provides several standard type classes that facilitate common operations across different types. Understanding these type classes is essential for effective programming in Haskell.

### 1. Eq

The `Eq` type class is used for types that support equality testing. It defines two functions: `(==)` for checking equality and `(/=)` for checking inequality.

#### Definition

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```

#### Example of Using Eq

1. **Making a Type an Instance of Eq**:
   ```haskell
   data Color = Red | Green | Blue

   instance Eq Color where
       Red == Red = True
       Green == Green = True
       Blue == Blue = True
       _ == _ = False
   ```

2. **Using Eq**:
   ```haskell
   main :: IO ()
   main = do
       print (Red == Green)  -- False
       print (Red == Red)    -- True
   ```

### 2. Ord

The `Ord` type class is for types that can be ordered. It provides functions for comparing values, such as `<`, `>`, `<=`, `>=`, and `compare`.

#### Definition

```haskell
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
```

#### Example of Using Ord

1. **Making a Type an Instance of Ord**:
   ```haskell
   data Color = Red | Green | Blue

   instance Ord Color where
       compare Red Green = LT
       compare Green Red = GT
       compare Red Blue = GT
       compare Blue Red = LT
       compare Green Blue = LT
       compare Blue Green = GT
       compare _ _ = EQ  -- Same colors are equal
   ```

2. **Using Ord**:
   ```haskell
   main :: IO ()
   main = do
       print (Red < Green)    -- True
       print (Blue > Green)   -- True
       print (Red == Red)     -- True
   ```

### 3. Show

The `Show` type class is used for types whose values can be converted to strings. It defines the `show` function, which returns a string representation of a value.

#### Definition

```haskell
class Show a where
    show :: a -> String
```

#### Example of Using Show

1. **Making a Type an Instance of Show**:
   ```haskell
   data Color = Red | Green | Blue

   instance Show Color where
       show Red = "Red"
       show Green = "Green"
       show Blue = "Blue"
   ```

2. **Using Show**:
   ```haskell
   main :: IO ()
   main = do
       print (show Red)      -- "Red"
       print (show Green)    -- "Green"
   ```

### 4. Read

The `Read` type class is for types that can be parsed from strings. It defines the `read` function, which converts a string back into a value of the specified type.

#### Definition

```haskell
class Read a where
    read :: String -> a
```

#### Example of Using Read

1. **Making a Type an Instance of Read**:
   ```haskell
   data Color = Red | Green | Blue deriving (Read, Show)

   -- No need to define read manually if deriving Read
   ```

2. **Using Read**:
   ```haskell
   main :: IO ()
   main = do
       let colorString = "Red"
       let color = read colorString :: Color
       print (show color)    -- "Red"
   ```

### Practical Example Combining All Four Type Classes

1. **Defining a Custom Data Type**:
   ```haskell
   data Person = Person { name :: String, age :: Int } deriving (Show, Read, Eq, Ord)

   -- Example instances are automatically derived
   ```

2. **Using the Person Type**:
   ```haskell
   main :: IO ()
   main = do
       let alice = Person "Alice" 30
       let bob = Person "Bob" 25

       print (show alice)                    -- "Person {name = \"Alice\", age = 30}"
       print (alice == bob)                  -- False
       print (alice > bob)                   -- True

       let personString = "Person {name = \"Charlie\", age = 35}"
       let charlie = read personString :: Person
       print (show charlie)                  -- "Person {name = \"Charlie\", age = 35}"
   ```

### Practice Exercises

1. Create a custom data type `Animal` with fields for `name` and `species`. Make it an instance of `Show`, `Read`, `Eq`, and `Ord`.

2. Write a program that reads a list of integers from a string, sorts them, and prints the sorted list using the `Show` type class.

3. Define a new type `Book` with fields for `title`, `author`, and `year`. Implement instances for `Show`, `Read`, `Eq`, and `Ord`.

### Conclusion

Understanding the standard type classes `Eq`, `Ord`, `Show`, and `Read` is crucial for effective programming in Haskell. These type classes provide essential functionality for comparing, displaying, and parsing values across different types. Practice implementing and using these type classes to become more proficient in Haskell!