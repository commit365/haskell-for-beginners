## Data Types: Int, Float, Double, Bool, Char

Haskell is a statically typed language with several basic data types. Let's explore the most common ones:

### 1. Int (Integer)

- Whole numbers, both positive and negative
- Has a fixed range (typically -2^63 to 2^63 - 1 on 64-bit systems)

```haskell
x :: Int
x = 42

y :: Int
y = -10
```

### 2. Float (Single-precision floating-point)

- Decimal numbers with single precision
- Less precise than Double, but uses less memory

```haskell
pi_approx :: Float
pi_approx = 3.14159
```

### 3. Double (Double-precision floating-point)

- Decimal numbers with double precision
- More precise than Float, but uses more memory

```haskell
precise_pi :: Double
precise_pi = 3.141592653589793
```

### 4. Bool (Boolean)

- Represents logical values
- Can only be `True` or `False`

```haskell
is_haskell_fun :: Bool
is_haskell_fun = True

is_easy :: Bool
is_easy = False
```

### 5. Char (Character)

- Represents a single Unicode character
- Enclosed in single quotes

```haskell
first_letter :: Char
first_letter = 'A'

smiley :: Char
smiley = '😊'
```

### Type Inference

Haskell can often infer types, so explicit type declarations aren't always necessary:

```haskell
auto_int = 42  -- Haskell infers this as Int
auto_double = 3.14  -- Haskell infers this as Double
auto_bool = True  -- Haskell infers this as Bool
auto_char = 'Z'  -- Haskell infers this as Char
```

### Checking Types in GHCi

Use `:type` or `:t` to check the type of an expression:

```haskell
Prelude> :t 42
42 :: Num p => p

Prelude> :t 3.14
3.14 :: Fractional p => p

Prelude> :t True
True :: Bool

Prelude> :t 'A'
'A' :: Char
```

### Practical Examples

```haskell
-- Function using multiple types
greet :: String -> Int -> String
greet name age = "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."

-- Using it
main :: IO ()
main = do
    putStrLn (greet "Alice" 30)
    let temperature :: Double
        temperature = 98.6
    putStrLn ("The temperature is " ++ show temperature ++ "°F")
```

### Practice

1. Create variables of each type and print them
2. Write a function that takes a `Float` and returns a `Bool` (e.g., is the temperature above freezing?)
3. Experiment with type inference by creating variables without explicit type declarations

Remember, understanding these basic types is crucial for Haskell programming. They form the foundation for more complex data structures and operations!