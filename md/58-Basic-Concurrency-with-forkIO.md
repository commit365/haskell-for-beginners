## Basic Concurrency with `forkIO`

Haskell provides a straightforward way to handle concurrency using the `Control.Concurrent` module. The `forkIO` function allows you to create lightweight threads that can run concurrently with the main program. This guide will cover the basics of using `forkIO` for concurrent programming in Haskell.

### Importing Necessary Modules

To work with concurrency in Haskell, you need to import the `Control.Concurrent` module:

```haskell
import Control.Concurrent
```

### Using `forkIO`

The `forkIO` function creates a new thread that runs concurrently with the main thread. It takes an `IO` action as an argument and returns a `ThreadId`, which can be used to manage the thread if needed.

#### Example: Simple Concurrent Threads

Here’s a basic example demonstrating how to use `forkIO` to run two threads concurrently:

```haskell
import Control.Concurrent

-- A simple function to print numbers
printNumbers :: String -> Int -> IO ()
printNumbers label n = do
    mapM_ (putStrLn . (label ++) . show) [1..n]

main :: IO ()
main = do
    -- Create two threads
    thread1 <- forkIO (printNumbers "Thread 1: " 5)
    thread2 <- forkIO (printNumbers "Thread 2: " 5)
    
    -- Wait for the threads to finish
    threadDelay 1000000  -- Delay for 1 second to allow threads to run
    putStrLn "Main thread finished."
```

### Explanation of the Example

1. **Function Definition**: The `printNumbers` function takes a label and a number `n`, then prints the numbers from 1 to `n` prefixed by the label.

2. **Creating Threads**: In the `main` function, we create two threads using `forkIO`, each running the `printNumbers` function with different labels.

3. **Thread Delay**: We use `threadDelay` to pause the main thread for 1 second, allowing the other threads to complete their execution before the main program finishes.

### Example Output

When you run the above program, you might see output similar to this:

```
Thread 1: 1
Thread 2: 1
Thread 1: 2
Thread 1: 3
Thread 2: 2
Thread 1: 4
Thread 2: 3
Thread 1: 5
Thread 2: 4
Thread 2: 5
Main thread finished.
```

### Important Notes

1. **Non-Deterministic Order**: The output from the threads may appear in a non-deterministic order due to the concurrent execution. This means that the order in which the numbers are printed may vary each time you run the program.

2. **Thread Safety**: When working with shared resources, you need to ensure thread safety. Haskell provides various mechanisms, such as `MVar` and `STM`, to manage shared state safely.

3. **Thread Management**: The `ThreadId` returned by `forkIO` can be used to manage threads, such as killing them if necessary (though this should be done carefully to avoid resource leaks).

### Summary

- **Concurrency in Haskell**: Haskell provides concurrency support through lightweight threads using `forkIO`.
- **Creating Threads**: Use `forkIO` to create concurrent threads that execute `IO` actions.
- **Non-Deterministic Execution**: The order of execution may vary, leading to non-deterministic output.

### Practice Exercises

1. Modify the example to create three threads that print different messages concurrently.
2. Implement a program that uses threads to download multiple web pages concurrently and prints their content.
3. Create a simple counter that increments a shared variable using multiple threads, ensuring thread safety.

### Conclusion

Basic concurrency in Haskell using `forkIO` allows you to run multiple threads concurrently, enabling you to perform tasks in parallel. Understanding how to use `forkIO` effectively will help you write more efficient and responsive Haskell programs. Practice these concepts to become proficient in concurrent programming with Haskell!