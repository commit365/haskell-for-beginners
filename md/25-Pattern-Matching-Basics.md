## Pattern Matching Basics

Pattern matching is a powerful feature in Haskell that allows you to deconstruct data types and bind variables to their components. It enhances the readability and expressiveness of your code by providing a clear way to handle different data structures.

### Basic Syntax

Pattern matching is typically used in function definitions, case expressions, and `let` bindings. The syntax generally follows this structure:

```haskell
functionName pattern = expression
```

### Simple Examples

1. **Matching a Single Value**:
   ```haskell
   isZero :: Int -> String
   isZero 0 = "Zero"
   isZero _ = "Not Zero"
   ```

2. **Matching Tuples**:
   ```haskell
   addTuple :: (Int, Int) -> Int
   addTuple (x, y) = x + y
   ```

3. **Matching Lists**:
   ```haskell
   headElement :: [a] -> a
   headElement (x:_) = x  -- Matches the head of the list
   headElement [] = error "Empty list"
   ```

### Pattern Matching with Data Types

1. **Using Pattern Matching with Custom Types**:
   ```haskell
   data Shape = Circle Float | Rectangle Float Float

   area :: Shape -> Float
   area (Circle r) = pi * r * r
   area (Rectangle w h) = w * h
   ```

2. **Multiple Constructors**:
   ```haskell
   data Person = Person { name :: String, age :: Int }

   greet :: Person -> String
   greet (Person n a) = "Hello, " ++ n ++ ", you are " ++ show a ++ " years old."
   ```

### Case Expressions

You can use `case` expressions for pattern matching in a more flexible way, especially when you have multiple patterns to match against.

1. **Using Case Expressions**:
   ```haskell
   describeShape :: Shape -> String
   describeShape shape = case shape of
       Circle r -> "Circle with radius " ++ show r
       Rectangle w h -> "Rectangle with width " ++ show w ++ " and height " ++ show h
   ```

### Nested Patterns

You can also use pattern matching to destructure nested data types.

1. **Nested Pattern Matching**:
   ```haskell
   data Point = Point Float Float

   distance :: Point -> Point -> Float
   distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
   ```

### Guards with Pattern Matching

You can combine pattern matching with guards to add additional conditions.

1. **Using Guards**:
   ```haskell
   describeAge :: Int -> String
   describeAge age
       | age < 13 = "Child"
       | age < 20 = "Teenager"
       | otherwise = "Adult"
   ```

### Practical Examples

1. **Factorial Function**:
   ```haskell
   factorial :: Int -> Int
   factorial 0 = 1
   factorial n = n * factorial (n - 1)
   ```

2. **List Length**:
   ```haskell
   listLength :: [a] -> Int
   listLength [] = 0
   listLength (_:xs) = 1 + listLength xs
   ```

### Practice Exercises

1. Write a function that takes a list and returns the last element using pattern matching.

2. Create a function that checks if a given list contains a specific element using pattern matching.

3. Implement a function that takes a custom data type representing a 2D point and returns the distance from the origin using pattern matching.

### Tips

- Use pattern matching to simplify your code and make it more readable.
- Always consider edge cases, such as empty lists or default cases, when using pattern matching.
- Pattern matching can be used with any data type, including tuples, lists, and custom types.

Pattern matching is a fundamental concept in Haskell that allows you to work with data structures in a clear and concise way. Practice using pattern matching in various scenarios to become comfortable with this powerful feature!