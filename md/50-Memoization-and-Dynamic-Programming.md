## Memoization and Dynamic Programming

**Memoization** is an optimization technique used to speed up recursive functions by storing the results of expensive function calls and reusing them when the same inputs occur again. **Dynamic programming** is a broader concept that utilizes memoization to solve complex problems by breaking them down into simpler subproblems.

### Memoization in Haskell

In Haskell, you can implement memoization using arrays or lists to cache results of function calls. This is especially useful for recursive functions like Fibonacci numbers or calculating factorials.

#### Example: Fibonacci Numbers

The naive recursive implementation of Fibonacci can be very slow due to repeated calculations. We can use memoization to optimize it.

1. **Naive Fibonacci Implementation**:

```haskell
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
```

2. **Memoized Fibonacci Implementation**:

We can use a list to store previously computed Fibonacci numbers.

```haskell
memoizedFibonacci :: Int -> Int
memoizedFibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)  -- Generate Fibonacci numbers
```

#### Example Usage of Memoized Fibonacci

```haskell
main :: IO ()
main = do
    print $ memoizedFibonacci 10  -- Output: 55
    print $ memoizedFibonacci 20  -- Output: 6765
```

### Dynamic Programming Example

Dynamic programming is often used to solve optimization problems. One classic example is the **0/1 Knapsack Problem**.

#### The 0/1 Knapsack Problem

Given a set of items, each with a weight and a value, determine the maximum value that can be carried in a knapsack of a given capacity.

1. **Dynamic Programming Approach**:

```haskell
knapsack :: Int -> [(Int, Int)] -> Int
knapsack capacity items = knapsackDP capacity items (length items)
  where
    knapsackDP _ _ 0 = 0
    knapsackDP cap items n
      | weight n > cap = knapsackDP cap items (n - 1)  -- Skip the item
      | otherwise = max (value n + knapsackDP (cap - weight n) items (n - 1))  -- Include the item
                                    (knapsackDP cap items (n - 1))  -- Exclude the item

    weight (i) = fst (items !! (i - 1))  -- Get weight of item i
    value (i) = snd (items !! (i - 1))   -- Get value of item i
```

2. **Example Usage of Knapsack Problem**:

```haskell
main :: IO ()
main = do
    let items = [(2, 3), (3, 4), (4, 5), (5, 6)]  -- (weight, value)
    let capacity = 5
    print $ knapsack capacity items  -- Output: 7 (items 1 and 2)
```

### Summary

- **Memoization**: A technique to store the results of expensive function calls to avoid redundant calculations.
- **Dynamic Programming**: A method for solving complex problems by breaking them down into simpler subproblems and storing their solutions.
- **Fibonacci Example**: Demonstrated memoization to optimize the Fibonacci calculation.
- **Knapsack Example**: Illustrated dynamic programming with the 0/1 Knapsack Problem.

### Practice Exercises

1. Implement a memoized version of the factorial function.
2. Solve the Longest Common Subsequence problem using dynamic programming.
3. Create a memoized version of the Collatz conjecture function.

### Conclusion

Understanding memoization and dynamic programming in Haskell can greatly enhance your ability to solve complex problems efficiently. Practice these concepts to become more proficient in algorithm design and optimization techniques!