## Creating and Using Modules

Modules in Haskell are a way to organize and encapsulate code. They allow you to group related functions, types, and type classes together, making your code more modular, reusable, and maintainable.

### Creating a Module

1. **Define a Module**:
   To create a module, you start with the `module` keyword followed by the module name and the `where` keyword. The module name should follow the Haskell naming conventions (typically capitalized).

   **Example**:
   ```haskell
   -- File: MathUtils.hs
   module MathUtils where

   add :: Int -> Int -> Int
   add x y = x + y

   multiply :: Int -> Int -> Int
   multiply x y = x * y
   ```

2. **Exporting Functions**:
   By default, all functions in a module are private. You can explicitly export functions by listing them after the `module` declaration.

   **Example**:
   ```haskell
   module MathUtils (add, multiply) where
   ```

   This means only `add` and `multiply` will be accessible to other modules.

### Importing a Module

To use the functions defined in a module, you need to import it into another module or your main program.

1. **Importing a Module**:
   Use the `import` keyword followed by the module name.

   **Example**:
   ```haskell
   -- File: Main.hs
   import MathUtils

   main :: IO ()
   main = do
       let sum = add 5 10
       let product = multiply 5 10
       putStrLn $ "Sum: " ++ show sum
       putStrLn $ "Product: " ++ show product
   ```

2. **Selective Imports**:
   You can import only specific functions from a module.

   **Example**:
   ```haskell
   import MathUtils (add)
   ```

3. **Hiding Imports**:
   If you want to import a module but exclude certain functions, you can use the `hiding` keyword.

   **Example**:
   ```haskell
   import MathUtils hiding (multiply)
   ```

### Organizing Modules

1. **Creating Submodules**:
   You can create submodules by organizing files in a directory structure. For example, if you have a directory named `Geometry`, you can create a module `Geometry.Circle`.

   **Example**:
   ```haskell
   -- File: Geometry/Circle.hs
   module Geometry.Circle where

   area :: Float -> Float
   area r = pi * r * r
   ```

   **Importing Submodules**:
   ```haskell
   import Geometry.Circle (area)
   ```

2. **Using Qualified Imports**:
   If you want to avoid name clashes between functions from different modules, you can use qualified imports.

   **Example**:
   ```haskell
   import qualified MathUtils as MU

   main :: IO ()
   main = do
       let sum = MU.add 5 10
       putStrLn $ "Sum: " ++ show sum
   ```

### Practical Example

1. **Creating a Simple Module**:
   ```haskell
   -- File: StringUtils.hs
   module StringUtils (toUpperCase, reverseString) where

   import Data.Char (toUpper)

   toUpperCase :: String -> String
   toUpperCase str = map toUpper str

   reverseString :: String -> String
   reverseString str = reverse str
   ```

2. **Using the Module**:
   ```haskell
   -- File: Main.hs
   import StringUtils

   main :: IO ()
   main = do
       let original = "hello"
       putStrLn $ "Original: " ++ original
       putStrLn $ "Uppercase: " ++ toUpperCase original
       putStrLn $ "Reversed: " ++ reverseString original
   ```

### Practice Exercises

1. Create a module named `ListUtils` that provides functions for common list operations (e.g., `isEmpty`, `length`, `append`).

2. Write a module named `Calculator` that exports functions for basic arithmetic operations (addition, subtraction, multiplication, division).

3. Create a submodule under `Geometry` for `Rectangle` that includes functions to calculate the area and perimeter.

### Tips

- Organize your code into modules to improve readability and maintainability.
- Use descriptive names for your modules and functions to clarify their purpose.
- Keep modules focused on a single responsibility to enhance reusability.

Creating and using modules in Haskell is essential for structuring your code effectively. Practice organizing your code into modules to become more proficient in Haskell programming!