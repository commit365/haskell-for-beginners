## Either for Error Handling with Messages

The `Either` type in Haskell is a powerful way to handle errors while providing informative messages. Unlike `Maybe`, which only indicates success or failure, `Either` allows you to convey additional information about the error.

### Definition of Either

The `Either` type is defined as follows:

```haskell
data Either a b = Left a | Right b
```

- `Left a` is typically used to represent an error or failure, containing an error message or code of type `a`.
- `Right b` represents a successful computation, containing a value of type `b`.

### Using Either for Error Handling

Using `Either` for error handling allows you to return detailed error messages along with the result of a computation.

#### Example: Safe Division

Let's create a function that performs division and uses `Either` to handle the case where the divisor is zero, returning an error message if the division fails.

```haskell
safeDivide :: Int -> Int -> Either String Float
safeDivide _ 0 = Left "Error: Division by zero"  -- Return Left with an error message
safeDivide x y = Right (fromIntegral x / fromIntegral y)  -- Return Right with the result
```

### Example Usage

You can use the `safeDivide` function in your main program and handle the `Either` result accordingly.

```haskell
main :: IO ()
main = do
    let result1 = safeDivide 10 2
    let result2 = safeDivide 10 0
    
    case result1 of
        Right value -> putStrLn $ "Result: " ++ show value  -- Output: Result: 5.0
        Left errorMessage -> putStrLn errorMessage  -- Output: (not executed)
    
    case result2 of
        Right value -> putStrLn $ "Result: " ++ show value  -- Output: (not executed)
        Left errorMessage -> putStrLn errorMessage  -- Output: Error: Division by zero
```

### Chaining Operations with Either

You can chain operations that return `Either` using the `>>=` (bind) operator. This allows you to handle errors gracefully without deeply nested case statements.

#### Example with Chaining

Let's create a function that takes two integers, divides them, and then doubles the result.

```haskell
doubleResult :: Int -> Int -> Either String Float
doubleResult x y = do
    result <- safeDivide x y  -- Perform the division
    return (result * 2)       -- Double the result
```

### Example Usage of Chaining

```haskell
main :: IO ()
main = do
    let result1 = doubleResult 10 2
    let result2 = doubleResult 10 0
    
    putStrLn $ "Result 1: " ++ show result1  -- Output: Result 1: Right 10.0
    putStrLn $ "Result 2: " ++ show result2  -- Output: Result 2: Left "Error: Division by zero"
```

### Using Either with Lists

You can also use `Either` in conjunction with lists to handle cases where a value might not be present and provide error messages.

#### Example: Finding an Element

Let's create a function that finds an element in a list and returns `Either` with an error message if the element is not found.

```haskell
findElement :: Eq a => a -> [a] -> Either String a
findElement _ [] = Left "Error: Element not found"  -- Return Left if the list is empty
findElement x xs = if x `elem` xs then Right x else Left "Error: Element not found"
```

### Example Usage of Finding an Element

```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 5]
    let found1 = findElement 3 numbers
    let found2 = findElement 6 numbers

    putStrLn $ "Found 3: " ++ show found1  -- Output: Found 3: Right 3
    putStrLn $ "Found 6: " ++ show found2  -- Output: Found 6: Left "Error: Element not found"
```

### Summary

- **Either Type**: A type that represents a value that can be one of two types, typically used for error handling where `Left` contains an error message and `Right` contains a successful result.
- **Error Handling**: Use `Either` to handle computations that can fail while providing informative error messages.
- **Chaining Operations**: Use the `>>=` operator or `do` notation to chain operations that return `Either`.
- **Combining with Lists**: Use `Either` to handle cases where a value might not be present in a list and provide error messages.

### Practice Exercises

1. Write a function that takes a list of integers and returns the first positive integer wrapped in `Either`. If there are no positive integers, return an error message.

2. Create a program that reads two integers from the user and uses `safeDivide` to divide them, handling the result with `Either` to display appropriate messages.

3. Implement a function that takes a list of strings and returns the first string that can be converted to an integer, using `Either` to handle conversion failures with an error message.

### Conclusion

Using the `Either` type for error handling in Haskell allows you to provide detailed error messages while maintaining type safety. This approach makes your code more robust and easier to understand. Practice these concepts to become more comfortable with using `Either` for error handling in Haskell!