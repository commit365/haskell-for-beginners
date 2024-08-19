## Applicative Functors: `pure` and `<*>`

**Applicative functors** extend the capabilities of functors by allowing you to apply functions that are themselves wrapped in a context (like a functor) to values that are also wrapped in a context. This makes it possible to work with multiple wrapped values in a more structured way.

### What is an Applicative Functor?

An **applicative functor** is a type class that provides two key functions: `pure` and `<*>`.

#### Applicative Type Class Definition

The `Applicative` type class is defined as follows:

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

- `pure` takes a value and wraps it in the applicative functor.
- `<*>` takes a wrapped function and a wrapped value, and applies the function to the value.

### Using `pure`

The `pure` function is used to lift a value into the context of the applicative functor.

#### Example with Lists

```haskell
main :: IO ()
main = do
    let wrappedValue = pure 5 :: [Int]  -- Lifts 5 into a list
    print wrappedValue  -- Output: [5]
```

### Using `<*>`

The `<*>` operator allows you to apply a function that is wrapped in a functor to a value that is also wrapped in a functor.

#### Example with Lists

```haskell
main :: IO ()
main = do
    let wrappedFunction = pure (*2) :: [Int -> Int]  -- Lifts the function into a list
    let wrappedValue = [1, 2, 3]
    let result = wrappedFunction <*> wrappedValue  -- Applies the function to each element
    print result  -- Output: [2, 4, 6]
```

### Example with `Maybe`

The `Maybe` type is another common example of an applicative functor.

#### Using `pure` with `Maybe`

```haskell
main :: IO ()
main = do
    let wrappedValue = pure 10 :: Maybe Int  -- Lifts 10 into a Maybe
    print wrappedValue  -- Output: Just 10
```

#### Using `<*>` with `Maybe`

```haskell
main :: IO ()
main = do
    let wrappedFunction = pure (+5) :: Maybe (Int -> Int)  -- Lifts the function into Maybe
    let wrappedValue = Just 10
    let result = wrappedFunction <*> wrappedValue  -- Applies the function to the value
    print result  -- Output: Just 15

    let nothingValue = Nothing
    let resultNothing = wrappedFunction <*> nothingValue  -- Applying to Nothing
    print resultNothing  -- Output: Nothing
```

### Example with Custom Data Types

You can also create your own data types that are instances of the `Applicative` type class.

#### Step 1: Define a Custom Data Type

```haskell
data Box a = Box a deriving (Show)
```

#### Step 2: Implement the Applicative Instance

```haskell
instance Functor Box where
    fmap f (Box x) = Box (f x)

instance Applicative Box where
    pure = Box
    (Box f) <*> (Box x) = Box (f x)
```

#### Step 3: Using Your Applicative Functor

```haskell
main :: IO ()
main = do
    let wrappedFunction = pure (*2) :: Box (Int -> Int)
    let wrappedValue = Box 10
    let result = wrappedFunction <*> wrappedValue
    print result  -- Output: Box 20
```

### Summary of Applicative Functors

- **Applicative Functor**: A type class that allows you to apply functions wrapped in a context to values wrapped in a context.
- **`pure`**: Lifts a value into the applicative functor.
- **`<*>`**: Applies a wrapped function to a wrapped value.

### Practice Exercises

1. Define a custom data type `Wrapper` that holds a value and implement the `Applicative` instance for it. Use `pure` and `<*>` to transform the value inside.

2. Create a list of `Maybe` values and use `pure` to lift a function that adds 10 to each value. Use `<*>` to apply this function to the `Maybe` values.

3. Implement an `Applicative` instance for a data type `Pair` that holds two values of the same type. Use `pure` and `<*>` to apply functions to both values.

### Conclusion

Applicative functors are a powerful abstraction in Haskell that allow you to work with functions and values wrapped in context. Understanding `pure` and `<*>` is essential for effective functional programming in Haskell. Practice using applicative functors to become more comfortable with this important concept!