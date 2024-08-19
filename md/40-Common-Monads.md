## Common Monads: Maybe, Either, List

Monads are a powerful abstraction in Haskell that allow you to work with values in a context. The `Maybe`, `Either`, and `List` monads are commonly used for handling optional values, error handling, and collections, respectively.

### 1. Maybe Monad

The `Maybe` monad is used to represent computations that may fail. It can either contain a value (`Just value`) or represent the absence of a value (`Nothing`).

#### Definition

```haskell
data Maybe a = Nothing | Just a
```

#### Using the Maybe Monad

1. **Example Function**: A function that safely divides two numbers.

```haskell
safeDivide :: Int -> Int -> Maybe Float
safeDivide _ 0 = Nothing  -- Division by zero
safeDivide x y = Just (fromIntegral x / fromIntegral y)
```

2. **Using Maybe with Do Notation**:

```haskell
calculate :: Int -> Int -> Maybe Float
calculate x y = do
    r1 <- safeDivide x y
    r2 <- safeDivide y x
    return (r1 + r2)
```

3. **Example Usage**:

```haskell
main :: IO ()
main = do
    let result = calculate 10 2
    case result of
        Just value -> putStrLn $ "Result: " ++ show value
        Nothing -> putStrLn "Error: Division by zero"
```

### 2. Either Monad

The `Either` monad is used for computations that can result in two possible outcomes: a success or a failure. It is often used for error handling, where `Left` represents an error and `Right` represents a successful value.

#### Definition

```haskell
data Either a b = Left a | Right b
```

#### Using the Either Monad

1. **Example Function**: A function that performs division and returns an error message if the division fails.

```haskell
safeDivideEither :: Int -> Int -> Either String Float
safeDivideEither _ 0 = Left "Division by zero"
safeDivideEither x y = Right (fromIntegral x / fromIntegral y)
```

2. **Using Either with Do Notation**:

```haskell
calculateEither :: Int -> Int -> Either String Float
calculateEither x y = do
    r1 <- safeDivideEither x y
    r2 <- safeDivideEither y x
    return (r1 + r2)
```

3. **Example Usage**:

```haskell
main :: IO ()
main = do
    let result = calculateEither 10 0
    case result of
        Right value -> putStrLn $ "Result: " ++ show value
        Left error -> putStrLn $ "Error: " ++ error
```

### 3. List Monad

The `List` monad represents non-deterministic computations, where a computation can have multiple results. It allows you to work with lists in a way that treats them as monadic values.

#### Definition

```haskell
data [] a = [] | a : [a]  -- List constructor
```

#### Using the List Monad

1. **Example Function**: Generating all possible pairs from two lists.

```haskell
pairs :: [Int] -> [Int] -> [(Int, Int)]
pairs xs ys = do
    x <- xs
    y <- ys
    return (x, y)
```

2. **Example Usage**:

```haskell
main :: IO ()
main = do
    let xs = [1, 2]
    let ys = [3, 4]
    let result = pairs xs ys
    print result  -- Output: [(1,3),(1,4),(2,3),(2,4)]
```

### Summary of Common Monads

- **Maybe Monad**: Represents computations that may fail, using `Just` for successful values and `Nothing` for failures.
- **Either Monad**: Used for computations that can result in two outcomes, where `Left` represents an error and `Right` represents a success.
- **List Monad**: Represents non-deterministic computations, allowing you to work with multiple results in a structured way.

### Practice Exercises

1. Write a function using the `Maybe` monad that reads an integer from the user and returns `Nothing` if the input is invalid.

2. Create a function using the `Either` monad that reads two integers and returns either their sum or an error message if either input is invalid.

3. Implement a function using the `List` monad that generates all combinations of a given list of colors and a list of sizes.

### Conclusion

Understanding the `Maybe`, `Either`, and `List` monads is essential for effective programming in Haskell. These monads provide powerful abstractions for handling optional values, error handling, and non-deterministic computations. Practice using these common monads to enhance your Haskell programming skills!