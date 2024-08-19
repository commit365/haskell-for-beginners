## Profiling Haskell Programs

Profiling is an essential technique for understanding the performance characteristics of your Haskell programs. It helps identify bottlenecks and memory usage issues, allowing you to optimize your code effectively. Haskell provides built-in support for profiling through GHC (Glasgow Haskell Compiler).

### Steps for Profiling Haskell Programs

Profiling a Haskell program generally involves three main steps:

1. **Compile the Program with Profiling Enabled**
2. **Run the Program with Profiling Options**
3. **Analyze the Profiling Output**

### 1. Compile the Program with Profiling Enabled

To enable profiling, you need to compile your Haskell program with specific flags. Use the `-prof` flag to enable profiling and `-fprof-auto` to automatically add cost centers to functions.

#### Example:

```bash
ghc -O2 -prof -fprof-auto -rtsopts MyProgram.hs
```

- `-O2`: Enables optimizations.
- `-prof`: Enables profiling.
- `-fprof-auto`: Automatically adds cost centers to all functions.
- `-rtsopts`: Allows passing runtime options to the program.

### 2. Run the Program with Profiling Options

After compiling your program, you can run it with runtime options to generate profiling information. The `+RTS` flag is used to specify runtime options, and `-p` generates a time profile.

#### Example:

```bash
./MyProgram +RTS -p -RTS
```

- `+RTS`: Starts the runtime system options.
- `-p`: Generates a profiling report in a file named `MyProgram.prof`.
- `-RTS`: Ends the runtime system options.

### 3. Analyze the Profiling Output

After running your program with profiling, you will have a `.prof` file containing the profiling data. This file includes information about time and memory usage for each cost center.

#### Example Output:

```
COST CENTRE      MODULE      SRC                 no. entries %time %alloc
...
mean             Main        MyProgram.hs:10:1   1000       60.0  50.0
sum              Main        MyProgram.hs:5:1    500       30.0  40.0
```

- **COST CENTRE**: The name of the function or expression being profiled.
- **%time**: The percentage of total execution time spent in this cost center.
- **%alloc**: The percentage of total memory allocated by this cost center.

### Using Cost Centers

You can manually add cost centers to specific functions using the `{-# SCC "name" #-}` pragma. This allows you to focus profiling on particular areas of your code.

#### Example:

```haskell
{-# SCC "mean" #-}
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
```

### Advanced Profiling Techniques

1. **Space Profiling**: Use the `-M` option to measure memory usage during program execution. This can help identify memory leaks or excessive memory consumption.

   ```bash
   ./MyProgram +RTS -M100m -RTS
   ```

2. **Ticky-Ticky Profiling**: This provides detailed allocation information. Compile with the `-ticky` flag and run with `-RTS -ticky`.

3. **Event Logging**: For more advanced profiling, you can enable event logging, which provides detailed information about the program's runtime behavior.

### Summary

- **Profiling**: A technique to analyze the performance of Haskell programs.
- **Compilation Flags**: Use `-prof`, `-fprof-auto`, and `-O2` to enable profiling.
- **Runtime Options**: Use `+RTS -p` to generate profiling reports.
- **Cost Centers**: Use `{-# SCC "name" #-}` to add manual cost centers for focused profiling.

### Practice Exercises

1. Write a Haskell program with intentional performance issues, profile it, and identify the bottlenecks.
2. Implement a function that uses a lot of memory and analyze its memory usage with profiling.
3. Experiment with different profiling flags and analyze how they affect the profiling output.

### Conclusion

Profiling is a crucial aspect of optimizing Haskell programs. By using GHC's profiling tools, you can gain insights into the performance of your code, identify bottlenecks, and make informed decisions for optimization. Practice these techniques to become proficient in profiling Haskell applications!
