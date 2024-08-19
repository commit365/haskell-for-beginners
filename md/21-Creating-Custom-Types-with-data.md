## Creating Custom Types with `data`

In Haskell, you can define your own data types using the `data` keyword. This allows you to create complex structures that can represent real-world entities and concepts.

### Basic Syntax

The syntax for defining a custom type is:

```haskell
data TypeName = Constructor1 | Constructor2 | Constructor3 ...
```

### Defining Simple Data Types

1. **Defining a Simple Data Type**:
   ```haskell
   data Color = Red | Green | Blue
   ```

2. **Using the Custom Type**:
   ```haskell
   myColor :: Color
   myColor = Red
   ```

### Defining Data Types with Parameters

You can also create parameterized types, which allow you to define types that can hold values of other types.

1. **Defining a Parameterized Type**:
   ```haskell
   data Pair a b = MkPair a b
   ```

2. **Using the Parameterized Type**:
   ```haskell
   myPair :: Pair Int String
   myPair = MkPair 1 "one"
   ```

### Defining Records

Records are a convenient way to define data types with named fields. This makes your data structures more readable and easier to work with.

1. **Defining a Record**:
   ```haskell
   data Person = Person { name :: String, age :: Int }
   ```

2. **Creating a Record Instance**:
   ```haskell
   alice :: Person
   alice = Person { name = "Alice", age = 30 }
   ```

3. **Accessing Record Fields**:
   ```haskell
   personName = name alice  -- "Alice"
   personAge = age alice    -- 30
   ```

### Defining Algebraic Data Types

Algebraic data types allow you to combine multiple constructors and parameterized types.

1. **Defining an Algebraic Data Type**:
   ```haskell
   data Shape = Circle Float          -- radius
              | Rectangle Float Float  -- width, height
   ```

2. **Using the Algebraic Data Type**:
   ```haskell
   myShape1 :: Shape
   myShape1 = Circle 5.0

   myShape2 :: Shape
   myShape2 = Rectangle 4.0 6.0
   ```

### Pattern Matching with Custom Types

Pattern matching allows you to deconstruct and work with your custom types easily.

1. **Using Pattern Matching**:
   ```haskell
   area :: Shape -> Float
   area (Circle r) = pi * r * r
   area (Rectangle w h) = w * h
   ```

2. **Example Usage**:
   ```haskell
   area myShape1  -- 78.53981633974483 (area of Circle with radius 5)
   area myShape2  -- 24.0 (area of Rectangle with width 4 and height 6)
   ```

### Creating Custom Types with Multiple Constructors

You can define a type with multiple constructors, each with different fields.

1. **Defining a Custom Type**:
   ```haskell
   data Vehicle = Car String Int | Bike String
   ```

2. **Using the Custom Type**:
   ```haskell
   myCar :: Vehicle
   myCar = Car "Toyota" 2020

   myBike :: Vehicle
   myBike = Bike "Trek"
   ```

3. **Pattern Matching on Multiple Constructors**:
   ```haskell
   vehicleInfo :: Vehicle -> String
   vehicleInfo (Car brand year) = "Car: " ++ brand ++ ", Year: " ++ show year
   vehicleInfo (Bike brand) = "Bike: " ++ brand
   ```

### Practice Exercises

1. Define a custom type `Book` with fields for title, author, and year published. Create a function that returns a formatted string for a `Book`.

2. Create a data type `Animal` with constructors for `Dog`, `Cat`, and `Bird`, each containing relevant information (e.g., name, age). Implement a function that describes the animal.

3. Define a type `Expression` that can represent either a number or a binary operation (addition, subtraction). Write a function to evaluate an `Expression`.

### Tips

- Use descriptive names for your types and constructors to improve code readability.
- Take advantage of pattern matching to simplify working with custom types.
- Consider using records when you have multiple related fields to improve clarity.

Creating custom types with the `data` keyword is a powerful feature in Haskell that allows you to model complex data structures. Practice defining and using custom types to become comfortable with this essential aspect of Haskell programming!