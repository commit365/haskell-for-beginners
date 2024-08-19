## Case Expressions

Case expressions in Haskell provide a way to perform pattern matching on values. They are useful for handling multiple patterns in a clean and organized manner. Case expressions can be used to destructure data types, including lists, tuples, and custom data types.

### Basic Syntax

The syntax for a case expression is as follows:

```haskell
case expression of
    pattern1 -> result1
    pattern2 -> result2
    ...
    _ -> defaultResult  -- Optional catch-all pattern
```

### Simple Examples

1. **Basic Case Expression**:
   ```haskell
   describeNumber :: Int -> String
   describeNumber n = case n of
       0 -> "Zero"
       1 -> "One"
       2 -> "Two"
       _ -> "Something else"  -- Catch-all pattern
   ```

2. **Using Case with Tuples**:
   ```haskell
   addTuple :: (Int, Int) -> Int
   addTuple pair = case pair of
       (x, y) -> x + y
   ```

3. **Using Case with Lists**:
   ```haskell
   firstElement :: [a] -> a
   firstElement list = case list of
       (x:_) -> x  -- Matches the head of the list
       []    -> error "Empty list"
   ```

### Case Expressions with Custom Data Types

1. **Defining a Custom Data Type**:
   ```haskell
   data Shape = Circle Float | Rectangle Float Float
   ```

2. **Using Case Expressions with Custom Types**:
   ```haskell
   area :: Shape -> Float
   area shape = case shape of
       Circle r -> pi * r * r
       Rectangle w h -> w * h
   ```

### Nested Case Expressions

You can nest case expressions for more complex data structures.

1. **Nested Case Example**:
   ```haskell
   data Point = Point Float Float

   describePoint :: Point -> String
   describePoint (Point x y) = case (x, y) of
       (0, 0) -> "Origin"
       (_, 0) -> "On the X-axis"
       (0, _) -> "On the Y-axis"
       _       -> "Somewhere else"
   ```

### Using Case Expressions in Let Bindings

You can use case expressions within `let` bindings to destructure values.

1. **Example with Let**:
   ```haskell
   describeShape :: Shape -> String
   describeShape shape =
       let areaValue = case shape of
               Circle r -> pi * r * r
               Rectangle w h -> w * h
       in "Area: " ++ show areaValue
   ```

### Practical Examples

1. **Weather Description**:
   ```haskell
   data Weather = Sunny | Rainy | Cloudy

   describeWeather :: Weather -> String
   describeWeather weather = case weather of
       Sunny  -> "It's a sunny day!"
       Rainy  -> "Don't forget your umbrella!"
       Cloudy -> "It might rain later."
   ```

2. **Traffic Light**:
   ```haskell
   data TrafficLight = Red | Yellow | Green

   trafficAction :: TrafficLight -> String
   trafficAction light = case light of
       Red    -> "Stop"
       Yellow -> "Caution"
       Green  -> "Go"
   ```

### Practice Exercises

1. Write a case expression that takes an `Int` and returns "Positive", "Negative", or "Zero" based on the value.

2. Create a custom data type `Animal` with constructors for `Dog`, `Cat`, and `Bird`. Write a function that describes the animal using a case expression.

3. Implement a case expression that matches a list of integers and returns the sum of positive numbers, ignoring negative numbers.

### Tips

- Use case expressions to handle multiple patterns in a clear and organized way.
- Remember that the underscore `_` can be used as a wildcard pattern to match any value.
- Case expressions can be nested, allowing for complex pattern matching on composite data structures.

Case expressions are a powerful tool in Haskell for handling different data patterns. They provide a clean and expressive way to work with various data types and simplify your code. Practice using case expressions in different scenarios to become comfortable with this essential feature!