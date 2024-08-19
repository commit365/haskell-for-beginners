## Working with Files

In Haskell, you can perform file operations such as reading from and writing to files using the `System.IO` module. This allows you to handle data persistence in your applications.

### Importing Necessary Modules

To work with files, you need to import the `System.IO` module:

```haskell
import System.IO
```

### Writing to a File

You can write data to a file using the `writeFile` or `appendFile` functions.

1. **Using `writeFile`**:
   The `writeFile` function creates a new file or overwrites an existing file with the specified content.

   **Example**:
   ```haskell
   main :: IO ()
   main = do
       writeFile "output.txt" "Hello, File!"  -- Write to a file
       putStrLn "Data written to output.txt"
   ```

2. **Using `appendFile`**:
   The `appendFile` function adds content to the end of an existing file without overwriting it.

   **Example**:
   ```haskell
   main :: IO ()
   main = do
       appendFile "output.txt" "\nAppending this line."  -- Append to the file
       putStrLn "Data appended to output.txt"
   ```

### Reading from a File

You can read the contents of a file using the `readFile` function.

1. **Using `readFile`**:
   The `readFile` function reads the entire content of a file and returns it as a string.

   **Example**:
   ```haskell
   main :: IO ()
   main = do
       contents <- readFile "output.txt"  -- Read the file contents
       putStrLn contents  -- Print the contents to the console
   ```

### Working with File Handles

For more complex file operations, you can use file handles. This allows you to open a file, perform multiple read/write operations, and then close the file.

1. **Opening a File**:
   Use `openFile` to get a file handle.

   **Example**:
   ```haskell
   main :: IO ()
   main = do
       handle <- openFile "output.txt" ReadMode  -- Open the file for reading
       contents <- hGetContents handle  -- Get the contents of the file
       putStrLn contents  -- Print the contents
       hClose handle  -- Close the file handle
   ```

2. **Writing with File Handles**:
   You can also write to a file using file handles.

   **Example**:
   ```haskell
   main :: IO ()
   main = do
       handle <- openFile "output.txt" WriteMode  -- Open the file for writing
       hPutStrLn handle "Writing this line to the file."  -- Write a line to the file
       hClose handle  -- Close the file handle
   ```

### Handling Exceptions

When working with files, it’s a good practice to handle exceptions, such as when a file does not exist or cannot be opened.

1. **Using `Control.Exception`**:
   You can use the `Control.Exception` module to catch exceptions.

   **Example**:
   ```haskell
   import System.IO
   import Control.Exception (catch, IOException)

   main :: IO ()
   main = do
       handle <- openFile "nonexistent.txt" ReadMode `catch` handler
       contents <- hGetContents handle
       putStrLn contents
       hClose handle
   where
       handler :: IOException -> IO Handle
       handler _ = do
           putStrLn "File not found. Creating a new file."
           writeFile "nonexistent.txt" "This is a new file."
           openFile "nonexistent.txt" ReadMode
   ```

### Summary of Working with Files

- **Writing to a File**: Use `writeFile` to create or overwrite a file and `appendFile` to add content to an existing file.
- **Reading from a File**: Use `readFile` to read the entire contents of a file.
- **File Handles**: Use `openFile`, `hGetContents`, `hPutStrLn`, and `hClose` for more complex file operations.
- **Exception Handling**: Use `Control.Exception` to manage errors when working with files.

### Practice Exercises

1. Write a program that prompts the user for their favorite color and writes it to a file. Then, read the file and print the content.

2. Create a program that reads a list of numbers from a file, calculates their average, and writes the result to another file.

3. Implement a program that appends user input to a file until the user types "exit". After exiting, read the file and print its contents.

### Conclusion

Working with files in Haskell allows you to read and write data persistently. Understanding how to use file operations effectively will help you build applications that can store and retrieve data. Practice these concepts to become more comfortable with file handling in Haskell!