## Higher-Order Functions

Higher-order functions are functions that can take functions as arguments or return functions as results. They are a powerful feature in Haskell, enabling more abstract and reusable code.

### Basic Concept

A higher-order function either:
1. Takes one or more functions as arguments, or
2. Returns a function as its result, or
3. Both of the above

### Common Higher-Order Functions

1. `map`: Apply a function to each element of a list
   ```haskell
   map :: (a -> b) -> [a] -> [b]
   map (*2) [1,2,3,4]  -- [2,4,6,8]
   ```

2. `filter`: Keep elements that satisfy a predicate
   ```haskell
   filter :: (a -> Bool) -> [a] -> [a]
   filter even [1,2,3,4,5]  -- [2,4]
   ```

3. `foldr`: Right-associative fold of a list
   ```haskell
   foldr :: (a -> b -> b) -> b -> [a] -> b
   foldr (+) 0 [1,2,3,4]  -- 10
   ```

4. `foldl`: Left-associative fold of a list
   ```haskell
   foldl :: (b -> a -> b) -> b -> [a] -> b
   foldl (+) 0 [1,2,3,4]  -- 10
   ```

### Creating Higher-Order Functions

1. Function that returns a function:
   ```haskell
   multiplyBy :: Int -> (Int -> Int)
   multiplyBy n = (\x -> n * x)

   let double = multiplyBy 2
   double 5  -- 10
   ```

2. Function that takes a function as an argument:
   ```haskell
   applyTwice :: (a -> a) -> a -> a
   applyTwice f x = f (f x)

   applyTwice (*2) 3  -- 12
   ```

### Practical Examples

1. Custom mapping function:
   ```haskell
   myMap :: (a -> b) -> [a] -> [b]
   myMap _ [] = []
   myMap f (x:xs) = f x : myMap f xs
   ```

2. Composition of functions:
   ```haskell
   compose :: (b -> c) -> (a -> b) -> (a -> c)
   compose f g = \x -> f (g x)

   let addOneAndDouble = compose (*2) (+1)
   addOneAndDouble 3  -- 8
   ```

3. Partial application:
   ```haskell
   addThree :: Int -> Int -> Int -> Int
   addThree x y z = x + y + z

   let addFive = addThree 2 3
   addFive 4  -- 9
   ```

### Advanced Examples

1. Currying:
   ```haskell
   curry :: ((a, b) -> c) -> a -> b -> c
   curry f x y = f (x, y)

   uncurry :: (a -> b -> c) -> (a, b) -> c
   uncurry f (x, y) = f x y
   ```

2. Function application operator:
   ```haskell
   ($) :: (a -> b) -> a -> b
   f $ x = f x

   sum $ map (*2) [1,2,3]  -- 12
   ```

3. Function composition operator:
   ```haskell
   (.) :: (b -> c) -> (a -> b) -> a -> c
   (f . g) x = f (g x)

   let doubleAndAddOne = (+1) . (*2)
   doubleAndAddOne 3  -- 7
   ```

### Practice Exercises

1. Implement your own version of `filter` using `foldr`.
2. Write a higher-order function that applies a list of functions to a value.
3. Create a function that takes a comparison function and returns a sorting function.

Remember, higher-order functions are a cornerstone of functional programming in Haskell. They allow for more abstract, reusable, and composable code. Practice using and creating higher-order functions to fully leverage the power of Haskell's functional paradigm!