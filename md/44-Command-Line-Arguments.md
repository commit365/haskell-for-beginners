## Command Line Arguments

In Haskell, you can access command line arguments using the `System.Environment` module. This allows your program to accept input directly from the command line when it is executed.

### Importing the Necessary Module

To work with command line arguments, you need to import the `System.Environment` module:

```haskell
import System.Environment (getArgs)
```

### Accessing Command Line Arguments

The `getArgs` function retrieves the command line arguments as a list of strings. This list does not include the program name; it only contains the arguments passed to the program.

#### Example Program

Here’s a simple example that demonstrates how to read command line arguments:

```haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs  -- Get the command line arguments
    putStrLn "Command line arguments:"
    mapM_ putStrLn args  -- Print each argument on a new line
```

### Running the Program

To run the above program, you would compile it (e.g., `ghc MyProgram.hs`) and then execute it from the command line with some arguments:

```bash
./MyProgram arg1 arg2 arg3
```

The output would be:

```
Command line arguments:
arg1
arg2
arg3
```

### Example: Using Command Line Arguments

Let’s create a program that takes two numbers as command line arguments, adds them together, and prints the result.

#### Example Program

```haskell
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs  -- Get command line arguments
    case args of
        [num1, num2] -> do
            let maybeNum1 = readMaybe num1 :: Maybe Int  -- Convert first argument
            let maybeNum2 = readMaybe num2 :: Maybe Int  -- Convert second argument
            case (maybeNum1, maybeNum2) of
                (Just n1, Just n2) -> putStrLn $ "The sum is: " ++ show (n1 + n2)
                _                  -> putStrLn "Please provide two valid integers."
        _ -> putStrLn "Please provide exactly two arguments."
```

### Running the Example Program

Compile and run the program with two integer arguments:

```bash
./MyProgram 5 10
```

The output will be:

```
The sum is: 15
```

If you provide invalid input:

```bash
./MyProgram 5 hello
```

The output will be:

```
Please provide two valid integers.
```

### Summary of Command Line Arguments

- **Importing**: Use `System.Environment` to access command line arguments.
- **Getting Arguments**: Use `getArgs` to retrieve a list of arguments.
- **Processing Arguments**: You can convert the arguments from strings to other types using `readMaybe` or similar functions.
- **Error Handling**: Handle cases where the wrong number of arguments or invalid input is provided.

### Practice Exercises

1. Write a program that takes a list of names as command line arguments and prints a greeting for each name.

2. Create a program that accepts a filename as a command line argument, reads the contents of the file, and prints them to the console.

3. Implement a program that takes a list of integers as command line arguments and calculates their average.

### Conclusion

Working with command line arguments in Haskell allows you to create flexible and interactive programs. Understanding how to access and process these arguments will enhance your ability to build command-line applications. Practice these concepts to become more comfortable with handling command line input in Haskell!