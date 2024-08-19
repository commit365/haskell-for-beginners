## Functors: `fmap` and `<$>`

In Haskell, a **functor** is a type class that allows you to apply a function over wrapped values. Functors are a fundamental concept in functional programming, enabling you to work with data structures in a consistent way.

### What is a Functor?

A type is a functor if it implements the `Functor` type class, which requires defining the `fmap` function. The `fmap` function applies a function to the value(s) inside a functor.

#### Functor Type Class Definition

The `Functor` type class is defined as follows:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

- `f` is the functor type constructor.
- `a` is the type of the value inside the functor.
- `b` is the type of the value after applying the function.

### Using `fmap`

The `fmap` function takes a function and a functor, applies the function to the value(s) inside the functor, and returns a new functor with the transformed value(s).

#### Example with Lists

Lists are a common example of a functor in Haskell.

```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4]
    let doubled = fmap (*2) numbers  -- Apply the function to each element
    print doubled  -- Output: [2, 4, 6, 8]
```

### Using `<$>`

The operator `<$>` is an infix synonym for `fmap`. It allows you to apply a function to a functor more concisely.

#### Example with `<$>`

```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4]
    let doubled = (*2) <$> numbers  -- Using <$>
    print doubled  -- Output: [2, 4, 6, 8]
```

### Functors with Other Data Types

You can also use `fmap` and `<$>` with other functor types, such as `Maybe`.

#### Example with `Maybe`

```haskell
main :: IO ()
main = do
    let justFive = Just 5
    let doubled = fmap (*2) justFive  -- Using fmap
    print doubled  -- Output: Just 10

    let nothing = Nothing
    let result = fmap (*2) nothing
    print result  -- Output: Nothing

    let doubledUsingInfix = (*2) <$> justFive  -- Using <$>
    print doubledUsingInfix  -- Output: Just 10
```

### Functors with Custom Data Types

You can also make your own data types instances of the `Functor` type class.

#### Step 1: Define a Custom Data Type

```haskell
data Box a = Box a
```

#### Step 2: Implement the Functor Instance

```haskell
instance Functor Box where
    fmap f (Box x) = Box (f x)
```

#### Step 3: Using Your Functor

```haskell
main :: IO ()
main = do
    let myBox = Box 10
    let newBox = fmap (*2) myBox  -- Using fmap
    print newBox  -- Output: Box 20

    let newBoxInfix = (*2) <$> myBox  -- Using <$>
    print newBoxInfix  -- Output: Box 20
```

### Summary of Functors

- **Functor**: A type class that allows you to apply a function to wrapped values.
- **fmap**: A function that applies a function to a functor.
- **<$>**: An infix operator that is equivalent to `fmap`.

### Practice Exercises

1. Define a custom data type `Wrapper` that holds a value and implement the `Functor` instance for it. Use `fmap` and `<$>` to transform the value inside.

2. Create a list of `Maybe` values and use `fmap` to apply a function that doubles the value inside each `Just`. Handle the `Nothing` cases appropriately.

3. Implement a `Functor` instance for a data type `Pair` that holds two values of the same type. Use `fmap` to apply a function to both values.

### Conclusion

Functors are a powerful abstraction in Haskell that allow you to work with values wrapped in context (like lists, `Maybe`, or custom types) in a consistent way. Understanding `fmap` and `<$>` is essential for functional programming in Haskell. Practice using functors to become more comfortable with this important concept!