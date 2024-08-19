## Reading from and Writing to the Console

In Haskell, you can interact with the console using IO operations. This includes reading user input and writing output to the console. The primary functions used for these tasks are `putStrLn`, `putStr`, and `getLine`.

### Writing to the Console

1. **Using `putStrLn`**:
   The `putStrLn` function prints a string to the console followed by a newline.

   **Example**:
   ```haskell
   main :: IO ()
   main = do
       putStrLn "Hello, World!"  -- Prints "Hello, World!" followed by a newline
   ```

2. **Using `putStr`**:
   The `putStr` function prints a string to the console without adding a newline at the end.

   **Example**:
   ```haskell
   main :: IO ()
   main = do
       putStr "Enter your name: "  -- Prompts without a newline
   ```

### Reading from the Console

1. **Using `getLine`**:
   The `getLine` function reads a line of input from the console and returns it as a string wrapped in the `IO` type.

   **Example**:
   ```haskell
   main :: IO ()
   main = do
       putStr "Enter your name: "
       name <- getLine  -- Reads user input
       putStrLn ("Hello, " ++ name ++ "!")  -- Greets the user
   ```

### Combining Reading and Writing

You can combine reading from and writing to the console in a single program using `do` notation.

#### Example Program

Here’s a complete example that reads a user's name and age, then prints a greeting:

```haskell
main :: IO ()
main = do
    putStr "Enter your name: "
    name <- getLine  -- Read the user's name
    putStr "Enter your age: "
    age <- getLine  -- Read the user's age
    putStrLn ("Hello, " ++ name ++ "! You are " ++ age ++ " years old.")
```

### Handling Input and Output

You can also handle different types of input by converting strings to other types, such as integers.

#### Example with Type Conversion

```haskell
import Text.Read (readMaybe)

main :: IO ()
main = do
    putStr "Enter your age: "
    ageInput <- getLine  -- Read input as a string
    case readMaybe ageInput :: Maybe Int of  -- Attempt to convert to Int
        Just age -> putStrLn ("You are " ++ show age ++ " years old.")
        Nothing -> putStrLn "Invalid age entered."
```

### Summary

- **Writing to the Console**: Use `putStrLn` for output with a newline and `putStr` for output without a newline.
- **Reading from the Console**: Use `getLine` to read user input as a string.
- **Combining IO Operations**: Use `do` notation to sequence multiple input and output operations.
- **Type Conversion**: Use functions like `readMaybe` to convert string input to other types safely.

### Practice Exercises

1. Write a program that asks the user for their favorite color and then prints a message including that color.

2. Create a program that reads two numbers from the user, adds them together, and prints the result.

3. Implement a program that prompts the user for a list of names, stores them in a list, and then prints each name on a new line.

### Conclusion

Reading from and writing to the console is a fundamental part of many Haskell programs. Understanding how to use these IO operations will help you build interactive applications. Practice these concepts to become more comfortable with console input and output in Haskell!