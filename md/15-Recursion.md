## Recursion: Basic Concepts and Examples

Recursion is a fundamental concept in functional programming where a function calls itself to solve a problem. It's particularly powerful and elegant in Haskell.

### Basic Concept

A recursive function has two main parts:
1. Base case(s): The condition(s) where the function returns a value without calling itself.
2. Recursive case(s): Where the function calls itself with a modified input.

### Simple Examples

1. Factorial:
   ```haskell
   factorial :: Integer -> Integer
   factorial 0 = 1                    -- Base case
   factorial n = n * factorial (n-1)  -- Recursive case
   ```

2. Sum of a list:
   ```haskell
   sumList :: Num a => [a] -> a
   sumList [] = 0                     -- Base case
   sumList (x:xs) = x + sumList xs    -- Recursive case
   ```

3. Length of a list:
   ```haskell
   listLength :: [a] -> Int
   listLength [] = 0                  -- Base case
   listLength (_:xs) = 1 + listLength xs  -- Recursive case
   ```

### More Complex Examples

1. Quicksort:
   ```haskell
   quicksort :: Ord a => [a] -> [a]
   quicksort [] = []                                   -- Base case
   quicksort (x:xs) = 
       let smallerSorted = quicksort [a | a <- xs, a <= x]
           biggerSorted = quicksort [a | a <- xs, a > x]
       in  smallerSorted ++ [x] ++ biggerSorted        -- Recursive case
   ```

2. Fibonacci sequence:
   ```haskell
   fibonacci :: Int -> Int
   fibonacci 0 = 0                                     -- Base case
   fibonacci 1 = 1                                     -- Base case
   fibonacci n = fibonacci (n-1) + fibonacci (n-2)     -- Recursive case
   ```

3. Binary tree traversal:
   ```haskell
   data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

   inorder :: Tree a -> [a]
   inorder EmptyTree = []                              -- Base case
   inorder (Node v left right) = 
       inorder left ++ [v] ++ inorder right            -- Recursive case
   ```

### Tail Recursion

Tail recursion is a special form of recursion where the recursive call is the last operation in the function. It can be optimized by the compiler.

Example: Tail-recursive factorial
```haskell
factorialTail :: Integer -> Integer -> Integer
factorialTail acc 0 = acc                              -- Base case
factorialTail acc n = factorialTail (acc * n) (n - 1)  -- Recursive case

factorial :: Integer -> Integer
factorial n = factorialTail 1 n
```

### Practical Tips

1. Always define base case(s) first to avoid infinite recursion.
2. Ensure that recursive calls move towards the base case.
3. Use pattern matching to simplify recursive definitions.
4. Consider tail recursion for better performance in some cases.

### Practice Exercises

1. Write a recursive function to reverse a list.
2. Implement a recursive function to find the maximum element in a list.
3. Create a recursive function to generate the nth element of the Fibonacci sequence.

Remember, recursion is a powerful tool in Haskell, often leading to elegant and concise solutions. However, it's important to ensure that your recursive functions terminate and are efficient. Practice writing recursive functions to become comfortable with this fundamental concept in functional programming!