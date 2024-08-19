## IO Basics

In Haskell, **IO** (Input/Output) is a fundamental concept that allows you to interact with the outside world, such as reading from and writing to files, handling user input, and displaying output to the console. Haskell treats IO operations as a special type of computation that can be sequenced using the `IO` type.

### Understanding the IO Type

The `IO` type represents actions that can perform input and output. An `IO` action does not return a regular value but instead returns an action that, when executed, performs the specified IO operation.

#### Example of an IO Action

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

In this example, `putStrLn` is an IO action that prints a string to the console. The type signature `IO ()` indicates that this action performs IO and returns a unit type `()`.

### Common IO Functions

1. **Printing to the Console**:
   - `putStrLn :: String -> IO ()`: Prints a string followed by a newline.
   - `print :: Show a => a -> IO ()`: Prints a value that implements the `Show` type class.

   ```haskell
   main :: IO ()
   main = do
       putStrLn "Enter your name:"
       name <- getLine  -- Read user input
       putStrLn ("Hello, " ++ name ++ "!")
   ```

2. **Reading Input**:
   - `getLine :: IO String`: Reads a line of input from the user.

   ```haskell
   main :: IO ()
   main = do
       putStrLn "Please enter your age:"
       age <- getLine
       putStrLn ("You are " ++ age ++ " years old.")
   ```

3. **Working with Files**:
   - `readFile :: FilePath -> IO String`: Reads the contents of a file.
   - `writeFile :: FilePath -> String -> IO ()`: Writes a string to a file.

   ```haskell
   main :: IO ()
   main = do
       writeFile "output.txt" "Hello, File!"  -- Write to a file
       contents <- readFile "output.txt"       -- Read from a file
       putStrLn contents                        -- Print the file contents
   ```

### Sequencing IO Actions

You can sequence multiple IO actions using the `do` notation. Each action can bind its result to a variable for use in subsequent actions.

#### Example of Sequencing

```haskell
main :: IO ()
main = do
    putStrLn "What is your favorite color?"
    color <- getLine
    putStrLn ("Your favorite color is " ++ color ++ ".")
```

### Handling IO Errors

Haskell's IO operations can fail, and you may want to handle errors gracefully. You can use the `Control.Exception` module to catch exceptions during IO operations.

#### Example of Handling Exceptions

```haskell
import Control.Exception (catch, SomeException)

main :: IO ()
main = do
    contents <- readFile "nonexistent.txt" `catch` handler
    putStrLn contents
  where
    handler :: SomeException -> IO String
    handler _ = return "File not found."
```

### Summary of IO Basics

- **IO Type**: Represents actions that perform input and output.
- **Common Functions**: Use `putStrLn`, `print`, `getLine`, `readFile`, and `writeFile` for basic IO operations.
- **Do Notation**: Use `do` notation to sequence multiple IO actions.
- **Error Handling**: Use exception handling to manage errors in IO operations.

### Practice Exercises

1. Write a program that prompts the user for their name and age, then prints a message including both.

2. Create a program that reads a list of numbers from a file, calculates their sum, and prints the result.

3. Implement a program that writes a list of strings to a file and then reads the file back, printing each string to the console.

### Conclusion

Understanding the basics of IO in Haskell is essential for building applications that interact with users and the file system. Practice using IO operations to become more familiar with handling input and output in Haskell!