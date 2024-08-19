## Monads: `return` and `>>=`

In Haskell, a **monad** is a type class that represents computations as a series of steps. Monads provide a way to chain operations together, allowing you to work with values wrapped in a context (like `Maybe`, `List`, or custom types) in a structured way.

### What is a Monad?

A monad is defined by three main components:

1. A type constructor that defines the context (e.g., `Maybe`, `[]`, etc.).
2. A function `return` (or `pure` in the context of `Applicative`) that takes a value and wraps it in the monad.
3. A bind operator `>>=` that takes a monadic value and a function that returns a monadic value, chaining them together.

#### Monad Type Class Definition

The `Monad` type class is defined as follows:

```haskell
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```

### Using `return`

The `return` function takes a value and wraps it in the monadic context.

#### Example with `Maybe`

```haskell
main :: IO ()
main = do
    let wrappedValue = return 5 :: Maybe Int  -- Wraps 5 in Just
    print wrappedValue  -- Output: Just 5
```

### Using the Bind Operator `>>=`

The bind operator `>>=` is used to chain operations on monadic values. It takes a monadic value and a function that returns a monadic value, applying the function to the unwrapped value.

#### Example with `Maybe`

```haskell
main :: IO ()
main = do
    let maybeValue = Just 10
    let result = maybeValue >>= (\x -> Just (x * 2))  -- Using >>= to apply a function
    print result  -- Output: Just 20

    let nothingValue = Nothing
    let resultNothing = nothingValue >>= (\x -> Just (x * 2))
    print resultNothing  -- Output: Nothing
```

### Example with Lists

Lists are another common example of a monad in Haskell.

#### Using `return` with Lists

```haskell
main :: IO ()
main = do
    let wrappedList = return 5 :: [Int]  -- Wraps 5 in a list
    print wrappedList  -- Output: [5]
```

#### Using `>>=` with Lists

```haskell
main :: IO ()
main = do
    let listValue = [1, 2, 3]
    let result = listValue >>= (\x -> [x, x * 2])  -- Using >>= to apply a function
    print result  -- Output: [1, 2, 2, 4, 3, 6]
```

### Example with Custom Data Types

You can also create your own data types that are instances of the `Monad` type class.

#### Step 1: Define a Custom Data Type

```haskell
data Box a = Box a deriving (Show)
```

#### Step 2: Implement the Monad Instance

```haskell
instance Functor Box where
    fmap f (Box x) = Box (f x)

instance Applicative Box where
    pure = Box
    (Box f) <*> (Box x) = Box (f x)

instance Monad Box where
    return = Box
    (Box x) >>= f = f x
```

#### Step 3: Using Your Monad

```haskell
main :: IO ()
main = do
    let myBox = Box 10
    let result = myBox >>= (\x -> Box (x * 2))  -- Using >>= to apply a function
    print result  -- Output: Box 20
```

### Summary of Monads

- **Monad**: A type class that allows chaining operations on values wrapped in a context.
- **`return`**: Wraps a value in the monadic context.
- **`>>=`**: The bind operator that applies a function to the unwrapped value and returns a new monadic value.

### Practice Exercises

1. Define a custom data type `Wrapper` that holds a value and implement the `Monad` instance for it. Use `return` and `>>=` to transform the value inside.

2. Create a list of `Maybe` values and use `>>=` to apply a function that adds 10 to each value inside `Just`. Handle the `Nothing` cases appropriately.

3. Implement a `Monad` instance for a data type `Pair` that holds two values of the same type. Use `return` and `>>=` to apply functions to both values.

### Conclusion

Monads are a powerful abstraction in Haskell that allow you to work with values wrapped in context in a structured way. Understanding `return` and `>>=` is essential for effective functional programming in Haskell. Practice using monads to become more comfortable with this important concept!