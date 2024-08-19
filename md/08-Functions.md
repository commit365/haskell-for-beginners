## Functions: Definition and Application

Functions are fundamental in Haskell. They're easy to define and apply, forming the core of Haskell programming.

### Function Definition

Basic syntax:
```haskell
functionName arg1 arg2 ... = expression
```

Examples:

1. Simple function:
   ```haskell
   double x = x * 2
   ```

2. Multiple arguments:
   ```haskell
   add x y = x + y
   ```

3. Using guards:
   ```haskell
   absolute x
     | x < 0     = -x
     | otherwise = x
   ```

4. With pattern matching:
   ```haskell
   isEmpty [] = True
   isEmpty _  = False
   ```

### Function Application

To use a function, simply write its name followed by arguments:

```haskell
double 5        -- 10
add 3 4         -- 7
absolute (-10)  -- 10
isEmpty [1,2,3] -- False
```

### Higher-Order Functions

Functions can take other functions as arguments:

```haskell
applyTwice f x = f (f x)

applyTwice double 3  -- 12
```

### Anonymous Functions (Lambdas)

Create functions without naming them:

```haskell
\x -> x * 2

map (\x -> x * 2) [1,2,3]  -- [2,4,6]
```

### Partial Application

Haskell functions are curried, allowing partial application:

```haskell
add5 = add 5
add5 10  -- 15
```

### Composition

Combine functions with `.`:

```haskell
double . add 2 $ 3  -- 10
```

### Practical Examples

1. Calculate area of a circle:
   ```haskell
   circleArea radius = pi * radius ^ 2
   ```

2. Check if a number is prime:
   ```haskell
   isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2..sqrtN]
     where sqrtN = floor . sqrt . fromIntegral $ n
   ```

3. Apply a function to each element of a list:
   ```haskell
   applyToList f = map f
   
   doubleList = applyToList double
   doubleList [1,2,3]  -- [2,4,6]
   ```

### Practice

1. Write a function that takes a person's name and age, returning a greeting string.
2. Create a function that calculates the sum of squares for a list of numbers.
3. Implement a function that applies a given function twice to its argument.

Remember, functions in Haskell are powerful and flexible. They're the primary way to structure your code and solve problems. Practice defining and applying functions in various ways to become proficient in Haskell programming!