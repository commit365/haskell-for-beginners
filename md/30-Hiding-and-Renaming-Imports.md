## Hiding and Renaming Imports

In Haskell, you can control which functions and types you import from a module. This is useful for avoiding naming conflicts or reducing the number of imported functions to only those you need. You can also rename imports to avoid conflicts or for clarity.

### Hiding Imports

Hiding allows you to import everything from a module except for specific functions or types that you want to exclude.

#### Syntax for Hiding Imports

To hide specific functions, use the `hiding` keyword followed by the names of the functions you want to exclude.

**Example**:
```haskell
import Data.List hiding (sort, nub)
```

In this example, everything from the `Data.List` module is imported except for the `sort` and `nub` functions.

#### Practical Example of Hiding Imports

1. **Using Hiding in a Module**:
   ```haskell
   -- File: Main.hs
   import Data.List hiding (sort)

   main :: IO ()
   main = do
       let numbers = [3, 1, 2]
       print $ reverse numbers  -- Works fine
       -- print $ sort numbers  -- This would cause a compile error
   ```

### Renaming Imports

You can rename an imported function or type using the `as` keyword. This is useful for avoiding naming conflicts or for providing more meaningful names.

#### Syntax for Renaming Imports

To rename an import, use the `as` keyword followed by the new name you want to use.

**Example**:
```haskell
import qualified Data.List as L
```

This imports the `Data.List` module and allows you to use `L` as a prefix for all functions from that module.

#### Practical Example of Renaming Imports

1. **Using Renaming in a Module**:
   ```haskell
   -- File: Main.hs
   import qualified Data.List as L

   main :: IO ()
   main = do
       let numbers = [3, 1, 2]
       print $ L.sort numbers  -- Using the renamed import
       print $ L.nub [1, 2, 2, 3]  -- Using the renamed import
   ```

### Combining Hiding and Renaming

You can combine hiding and renaming in a single import statement.

**Example**:
```haskell
import Data.List hiding (sort) qualified as L
```

However, keep in mind that you cannot use both `hiding` and `qualified` in the same import statement. Instead, you can use separate import statements:

```haskell
import Data.List hiding (sort)
import qualified Data.List as L
```

### Practical Example of Hiding and Renaming

1. **Creating a Module**:
   Suppose you have a module named `MathOperations`.

   ```haskell
   -- File: MathOperations.hs
   module MathOperations (add, multiply, divide) where

   add :: Int -> Int -> Int
   add x y = x + y

   multiply :: Int -> Int -> Int
   multiply x y = x * y

   divide :: Int -> Int -> Int
   divide x y = x `div` y
   ```

2. **Using Hiding and Renaming in Main**:
   ```haskell
   -- File: Main.hs
   import MathOperations hiding (divide)
   import qualified MathOperations as MO

   main :: IO ()
   main = do
       let sum = add 5 10
       let product = multiply 5 10
       putStrLn $ "Sum: " ++ show sum
       putStrLn $ "Product: " ++ show product
       -- putStrLn $ "Division: " ++ show (divide 10 2)  -- This would cause a compile error
       putStrLn $ "Division using qualified import: " ++ show (MO.divide 10 2)
   ```

### Practice Exercises

1. Create a module named `StringUtils` that provides functions for string manipulation. Use hiding to exclude specific functions when importing it into your main program.

2. Write a program that imports the `Data.Map` module, renaming the `insert` function to `addEntry` to avoid conflicts with another `insert` function you have defined.

3. Define a module `Geometry` with functions for calculating the area of different shapes. Import it into a main program using hiding to exclude certain functions.

### Tips

- Use hiding to keep your namespace clean and avoid importing unnecessary functions.
- Use renaming to clarify the purpose of functions or to avoid naming conflicts.
- Keep your import statements organized at the top of your Haskell files for better readability.

Hiding and renaming imports are powerful features in Haskell that help you manage your code more effectively. Practice using these techniques to become more proficient in organizing and structuring your Haskell projects!