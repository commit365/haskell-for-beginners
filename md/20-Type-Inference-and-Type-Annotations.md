## Type Inference and Type Annotations

Haskell is a statically typed language, which means that the types of all expressions are known at compile time. However, Haskell also features type inference, allowing the compiler to automatically deduce the types of expressions without explicit type annotations.

### Type Inference

1. **Automatic Type Deduction**:
   Haskell can infer the type of expressions based on how they are used. For example:
   ```haskell
   add x y = x + y
   ```
   The compiler infers the type of `add` as:
   ```haskell
   add :: Num a => a -> a -> a
   ```

2. **Example of Type Inference**:
   ```haskell
   double x = x * 2
   -- The inferred type is:
   -- double :: Num a => a -> a
   ```

3. **Polymorphism**:
   Haskell allows functions to be polymorphic, meaning they can operate on any type that satisfies a certain constraint:
   ```haskell
   identity x = x
   -- The inferred type is:
   -- identity :: a -> a
   ```

### Type Annotations

1. **Adding Type Annotations**:
   You can explicitly specify the type of a function or variable using type annotations. This can improve code readability and help catch errors.
   ```haskell
   add :: Int -> Int -> Int
   add x y = x + y
   ```

2. **Function Type Annotations**:
   ```haskell
   square :: Num a => a -> a
   square x = x * x
   ```

3. **Variable Type Annotations**:
   ```haskell
   piValue :: Double
   piValue = 3.14159
   ```

4. **Using Type Annotations with Data Structures**:
   ```haskell
   myList :: [Int]
   myList = [1, 2, 3, 4, 5]
   ```

### Practical Examples

1. **Type Annotations with Higher-Order Functions**:
   ```haskell
   applyTwice :: (a -> a) -> a -> a
   applyTwice f x = f (f x)
   ```

2. **Type Annotations in Data Types**:
   ```haskell
   data Point = Point Int Int  -- Point is a data type with two Int fields

   move :: Point -> Point
   move (Point x y) = Point (x + 1) (y + 1)
   ```

3. **Type Annotations with Lists**:
   ```haskell
   sumList :: [Int] -> Int
   sumList [] = 0
   sumList (x:xs) = x + sumList xs
   ```

### Type Checking

1. **Type Errors**:
   If you provide an argument of the wrong type, Haskell will raise a type error:
   ```haskell
   add 5 "Hello"  -- Type error: No instance for (Num [Char])
   ```

2. **GHCi Type Checking**:
   You can check the type of an expression in GHCi using the `:type` or `:t` command:
   ```haskell
   Prelude> :t add
   add :: Num a => a -> a -> a
   ```

### Benefits of Type Annotations

1. **Improved Readability**:
   Type annotations make the code easier to understand for others (and yourself).

2. **Error Prevention**:
   Explicit types can help catch errors early in the development process.

3. **Documentation**:
   Type annotations serve as documentation for how functions and data structures are intended to be used.

### Practice Exercises

1. Write a function `multiply` that takes two parameters and returns their product, adding type annotations to specify that it works with any numeric type.

2. Create a data type `Person` with fields for name (String) and age (Int), and write a function that takes a `Person` and returns a greeting string.

3. Implement a function `filterEven` that filters out even numbers from a list of integers, and add type annotations to specify its input and output types.

### Tips

- Use type annotations when defining public APIs or when the type is not immediately clear from the context.
- Take advantage of Haskell's type inference to keep your code concise, but don't hesitate to add annotations for clarity.
- Familiarize yourself with common type classes (like `Num`, `Ord`, `Eq`) to understand how Haskell handles different types.

Understanding type inference and type annotations is crucial for effective Haskell programming. They help ensure code correctness and improve the maintainability of your codebase. Practice using both to become more comfortable with Haskell's type system!