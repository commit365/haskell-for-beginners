## Parallel Programming with `par` and `pseq`

Haskell provides mechanisms for parallel programming through the `Control.Parallel` module, which includes the `par` and `pseq` functions. These functions allow you to express parallel computations, enabling your Haskell programs to take advantage of multiple CPU cores.

### Importing Necessary Modules

To use `par` and `pseq`, you need to import the `Control.Parallel` module:

```haskell
import Control.Parallel (par, pseq)
```

### Understanding `par` and `pseq`

- **`par`**: This function sparks a new thread to evaluate its first argument in parallel with the second argument. It allows you to indicate that the first computation can be done concurrently.

  ```haskell
  par :: a -> b -> b
  ```

- **`pseq`**: This function ensures that its first argument is evaluated to weak head normal form (WHNF) before evaluating its second argument. It provides a strict ordering of evaluation, which is crucial when combined with `par`.

  ```haskell
  pseq :: a -> b -> b
  ```

### Example: Parallel Fibonacci Calculation

Let's implement a parallel version of the Fibonacci function using `par` and `pseq`. This example demonstrates how to leverage parallelism to compute Fibonacci numbers more efficiently.

```haskell
import Control.Parallel (par, pseq)

-- Parallel Fibonacci function
nfib :: Int -> Int
nfib n
    | n <= 1 = 1
    | otherwise = n1 `par` (n2 `pseq` (n1 + n2))
  where
    n1 = nfib (n - 1)
    n2 = nfib (n - 2)

main :: IO ()
main = do
    let result = nfib 40  -- Compute the 40th Fibonacci number
    print result
```

### Explanation of the Example

1. **Function Definition**: The `nfib` function computes the Fibonacci number for a given integer `n`. If `n` is less than or equal to 1, it returns 1.

2. **Using `par` and `pseq`**:
   - `n1` is computed in a separate thread using `par`, allowing it to run concurrently.
   - `n2` is evaluated strictly before the addition with `pseq`, ensuring that its value is ready when needed.

3. **Main Function**: The `main` function computes the 40th Fibonacci number and prints the result.

### Example Output

When you run the program, it will compute and display the 40th Fibonacci number:

```
102334155
```

### Performance Considerations

1. **Granularity**: The effectiveness of `par` depends on the granularity of the computations. If the computations are too small, the overhead of managing parallel threads may outweigh the benefits.

2. **Sparks**: When you use `par`, it creates a "spark" for the computation. The runtime system decides whether to execute the spark immediately or defer it based on available resources.

3. **Avoiding Fizzling**: If the main thread evaluates one of the arguments before the other is sparked, the computation may not run in parallel, leading to a loss of potential performance gains. Using `pseq` helps ensure that the necessary computations are performed in the correct order.

### Summary

- **Parallel Programming**: Haskell provides `par` and `pseq` for expressing parallel computations.
- **`par`**: Sparks a new thread for the first argument, allowing concurrent execution.
- **`pseq`**: Ensures the first argument is evaluated before the second, providing strict evaluation order.

### Practice Exercises

1. Modify the Fibonacci example to compute the Fibonacci of a larger number and observe the performance improvement.
2. Implement a parallel version of the quicksort algorithm using `par` and `pseq`.
3. Create a program that calculates the sum of a large list of numbers in parallel.

### Conclusion

Using `par` and `pseq` in Haskell allows you to write parallel programs that can efficiently utilize multiple CPU cores. By understanding how to express parallelism in your code, you can improve the performance of computationally intensive tasks. Practice these concepts to become proficient in parallel programming with Haskell!
