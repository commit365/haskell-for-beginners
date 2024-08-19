## Function Composition

Function composition is a technique for combining two or more functions into a single function. It's a powerful way to build complex functions from simpler ones.

### Basic Syntax

The function composition operator in Haskell is `.` (dot):

```haskell
(f . g) x = f (g x)
```

This means "apply g, then apply f to the result".

### Simple Examples

1. Basic composition:
   ```haskell
   import Data.Char (toUpper)

   exclaim :: String -> String
   exclaim = (++ "!")

   shout :: String -> String
   shout = exclaim . map toUpper

   shout "hello"  -- "HELLO!"
   ```

2. Multiple compositions:
   ```haskell
   import Data.List (sort)

   process :: [Int] -> Int
   process = sum . filter even . sort

   process [3,1,4,1,5,9,2,6,5,3,5]  -- 12
   ```

### Practical Uses

1. Creating pipelines:
   ```haskell
   pipeline :: String -> String
   pipeline = reverse . concat . words . map toUpper

   pipeline "hello world"  -- "DLROWOLLEH"
   ```

2. Point-free style:
   ```haskell
   -- Instead of:
   doubleAndAddOne x = (x * 2) + 1

   -- We can write:
   doubleAndAddOne = (+1) . (*2)
   ```

3. Working with Maybe:
   ```haskell
   import Data.Maybe (fromMaybe)

   safeProcess :: Maybe Int -> Int
   safeProcess = fromMaybe 0 . fmap (*2)

   safeProcess (Just 5)  -- 10
   safeProcess Nothing   -- 0
   ```

### Advanced Composition

1. Composing functions with multiple arguments:
   ```haskell
   combine :: (c -> d) -> (a -> b -> c) -> a -> b -> d
   combine f g x y = f (g x y)

   addThenDouble :: Int -> Int -> Int
   addThenDouble = combine (*2) (+)

   addThenDouble 3 4  -- 14
   ```

2. Composition with operators:
   ```haskell
   import Data.List (sortOn)

   sortByLength :: [[a]] -> [[a]]
   sortByLength = sortOn length

   sortByLengthDesc :: [[a]] -> [[a]]
   sortByLengthDesc = reverse . sortOn length
   ```

3. Composition in higher-order functions:
   ```haskell
   applyToSquare :: (Int -> Int) -> Int -> Int
   applyToSquare f = f . (^2)

   applyToSquare (+1) 4  -- 17
   ```

### Practice Exercises

1. Write a function `processString` that takes a string, converts it to lowercase, removes spaces, and reverses it, using function composition.

2. Create a function `sumOfSquaresOfOdds` that takes a list of integers, keeps only the odd numbers, squares them, and then sums the results, using function composition.

3. Implement a function `safeHead` that safely gets the first element of a list, returning `Nothing` for an empty list, and then use composition to create a function that doubles the first element of a list (or returns 0 for an empty list).

### Tips

- Use function composition to break complex operations into simpler, reusable parts.
- The `.` operator is right-associative, so `f . g . h` is equivalent to `f . (g . h)`.
- When composing many functions, consider using the `$` operator to avoid excessive parentheses: `f . g . h $ x` instead of `(f . g . h) x`.
- Remember that `f . g $ x` is equivalent to `f (g x)`.

Function composition is a key concept in functional programming, allowing you to build complex behavior from simple, reusable functions. Practice composing functions to write more elegant and maintainable code!