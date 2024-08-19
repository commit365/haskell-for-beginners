## Import Statements and Qualified Imports

In Haskell, import statements allow you to use functions and types defined in other modules. This is essential for organizing code and reusing libraries. Qualified imports help avoid naming conflicts when different modules export functions with the same name.

### Basic Import Statements

1. **Importing a Module**:
   To use the functions from a module, you can simply import it using the `import` keyword.

   **Example**:
   ```haskell
   import Data.List
   ```

   This statement imports all functions from the `Data.List` module.

2. **Selective Imports**:
   If you only need specific functions from a module, you can import them selectively.

   **Example**:
   ```haskell
   import Data.List (sort, nub)
   ```

   This imports only the `sort` and `nub` functions from the `Data.List` module.

3. **Hiding Imports**:
   If you want to import a module but exclude certain functions, you can use the `hiding` keyword.

   **Example**:
   ```haskell
   import Data.List hiding (sort)
   ```

   This imports everything from `Data.List` except the `sort` function.

### Qualified Imports

Qualified imports allow you to import a module with a prefix, which helps avoid naming conflicts when different modules export functions with the same name.

1. **Using Qualified Imports**:
   You can import a module qualified with a specific name.

   **Example**:
   ```haskell
   import qualified Data.List as L
   ```

   Now, you can use functions from `Data.List` with the prefix `L.`.

2. **Using Qualified Functions**:
   After importing a module qualified, you need to use the prefix to access its functions.

   **Example**:
   ```haskell
   main :: IO ()
   main = do
       let numbers = [3, 1, 2]
       print $ L.sort numbers  -- Using qualified import
       print $ L.nub [1, 2, 2, 3]  -- Using qualified import
   ```

3. **Combining Qualified and Selective Imports**:
   You can also combine qualified imports with selective imports.

   **Example**:
   ```haskell
   import qualified Data.List as L
   import Data.Char (toUpper)

   main :: IO ()
   main = do
       let str = "hello"
       print $ toUpper (head str)  -- Using unqualified import
       print $ L.sort [3, 1, 2]    -- Using qualified import
   ```

### Practical Example

1. **Creating a Module**:
   First, let’s create a simple module named `MathUtils`.

   ```haskell
   -- File: MathUtils.hs
   module MathUtils (add, multiply) where

   add :: Int -> Int -> Int
   add x y = x + y

   multiply :: Int -> Int -> Int
   multiply x y = x * y
   ```

2. **Using the Module with Import Statements**:
   Now, you can use this module in your main program.

   ```haskell
   -- File: Main.hs
   import MathUtils (add)  -- Selectively importing the add function

   main :: IO ()
   main = do
       let sum = add 5 10
       putStrLn $ "Sum: " ++ show sum
   ```

3. **Using Qualified Imports**:
   If you want to import `MathUtils` qualified, you can do so as follows:

   ```haskell
   -- File: Main.hs
   import qualified MathUtils as MU

   main :: IO ()
   main = do
       let sum = MU.add 5 10  -- Using qualified import
       putStrLn $ "Sum: " ++ show sum
   ```

### Practice Exercises

1. Create a module named `StringUtils` that provides functions for string manipulation (e.g., reversing a string, converting to uppercase). Import it into a main program using both selective and qualified imports.

2. Write a program that uses qualified imports to handle both `Data.List` and `Data.Set`, demonstrating the use of functions from both modules without naming conflicts.

3. Define a module `Geometry` with functions for calculating the area of different shapes. Import it into a main program and use both selective and qualified imports.

### Tips

- Use selective imports to keep your namespace clean and avoid importing unnecessary functions.
- Use qualified imports when you have naming conflicts between functions from different modules.
- Keep your import statements organized at the top of your Haskell files for better readability.

Understanding import statements and qualified imports is crucial for effective code organization and reuse in Haskell. Practice using these features to become more comfortable with module management in your Haskell projects!