## Introduction to GHCi and Basic Interactions

GHCi is the interactive environment for Haskell. It allows you to test code, evaluate expressions, and explore Haskell features quickly.

### Starting GHCi

1. Open your terminal or command prompt
2. Type `ghci` and press Enter
3. You should see a prompt like `Prelude>`

### Basic Interactions

1. **Simple Calculations**
   ```haskell
   Prelude> 2 + 2
   4
   Prelude> 50 * 100
   5000
   ```

2. **Using Functions**
   ```haskell
   Prelude> sqrt 16
   4.0
   Prelude> max 10 20
   20
   ```

3. **Defining Variables**
   ```haskell
   Prelude> let x = 5
   Prelude> x * 2
   10
   ```

4. **Creating Simple Functions**
   ```haskell
   Prelude> let double x = x * 2
   Prelude> double 7
   14
   ```

5. **Working with Lists**
   ```haskell
   Prelude> [1, 2, 3, 4]
   [1,2,3,4]
   Prelude> head [1, 2, 3]
   1
   Prelude> tail [1, 2, 3]
   [2,3]
   ```

6. **Using String Functions**
   ```haskell
   Prelude> "Hello" ++ " World"
   "Hello World"
   Prelude> length "Haskell"
   7
   ```

7. **Getting Type Information**
   ```haskell
   Prelude> :type 5
   5 :: Num p => p
   Prelude> :type "Hello"
   "Hello" :: [Char]
   ```

### Useful GHCi Commands

- `:quit` or `:q` - Exit GHCi
- `:load` or `:l` - Load a Haskell file
- `:reload` or `:r` - Reload the current file
- `:type` or `:t` - Show the type of an expression
- `:info` or `:i` - Display information about a function or type

### Practice

Try these exercises in GHCi:
1. Calculate the area of a circle with radius 5 (use `pi * r^2`)
2. Create a list of numbers from 1 to 10
3. Define a function that adds 1 to a number and test it

GHCi is a powerful tool for learning and experimenting with Haskell. Use it to test ideas and explore the language interactively!