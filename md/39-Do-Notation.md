## Do Notation

**Do notation** is a syntactic sugar in Haskell that makes working with monads more readable and easier to write. It allows you to sequence monadic operations in a way that resembles imperative programming, making it easier to understand the flow of computations.

### Basic Structure of Do Notation

The basic structure of do notation is as follows:

```haskell
do
    x <- action1
    y <- action2
    result <- action3 x y
    return result
```

- Each line can perform a monadic action and bind its result to a variable using the `<-` operator.
- You can then use these bound variables in subsequent actions.
- The final expression is returned using `return`.

### Example with `Maybe`

Let’s start with a simple example using the `Maybe` monad.

#### Without Do Notation

Here’s how you might write a function using the `Maybe` monad without do notation:

```haskell
safeDivide :: Int -> Int -> Maybe Float
safeDivide _ 0 = Nothing
safeDivide x y = Just (fromIntegral x / fromIntegral y)

calculate :: Int -> Int -> Maybe Float
calculate x y =
    let result1 = safeDivide x y
        result2 = safeDivide y x
    in case (result1, result2) of
        (Just r1, Just r2) -> Just (r1 + r2)
        _                  -> Nothing
```

#### With Do Notation

Now, let’s rewrite the `calculate` function using do notation:

```haskell
calculate :: Int -> Int -> Maybe Float
calculate x y = do
    r1 <- safeDivide x y
    r2 <- safeDivide y x
    return (r1 + r2)
```

### Example with Lists

Do notation can also be used with lists, which are another example of a monad.

#### Using Do Notation with Lists

```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3]
    let results = do
            x <- numbers
            return (x * 2)
    print results  -- Output: [2, 4, 6]
```

### Example with IO

Do notation is particularly useful when working with the `IO` monad, as it helps manage side effects in a clear way.

#### Using Do Notation with IO

```haskell
main :: IO ()
main = do
    putStrLn "Enter your name:"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")
```

### Combining Different Monads

You can also use do notation to work with multiple monads, such as `Maybe` and `IO`, by using `>>=` to chain operations.

#### Example of Combining `Maybe` and `IO`

```haskell
main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let number = readMaybe input :: Maybe Int
    result <- case number of
        Just n -> return (Just (n * 2))
        Nothing -> return Nothing
    case result of
        Just r -> putStrLn ("Double: " ++ show r)
        Nothing -> putStrLn "Invalid number"
```

### Summary of Do Notation

- **Do notation** simplifies the syntax for working with monads, making it more readable.
- You can bind results of monadic actions to variables using `<-`.
- The final expression in the do block is returned using `return`.

### Practice Exercises

1. Write a function using do notation that reads two numbers from the user, divides the first by the second, and prints the result. Handle division by zero using the `Maybe` monad.

2. Create a function that generates a list of squares for a list of numbers using do notation.

3. Implement a small program that reads a list of integers from the user, doubles each integer using do notation, and prints the results.

### Conclusion

Do notation is a powerful feature in Haskell that makes working with monads more intuitive and easier to read. Practice using do notation to become more comfortable with monadic operations and improve your Haskell programming skills!