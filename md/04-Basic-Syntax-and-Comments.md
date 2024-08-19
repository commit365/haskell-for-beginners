## Basic Syntax and Comments

Haskell has a clean, minimalist syntax. Let's cover the essentials:

### Basic Syntax

1. **Function Definition**
   ```haskell
   functionName arg1 arg2 = result
   ```
   Example:
   ```haskell
   add x y = x + y
   ```

2. **Function Application**
   ```haskell
   functionName arg1 arg2
   ```
   Example:
   ```haskell
   add 3 4  -- Returns 7
   ```

3. **Infix Operators**
   ```haskell
   arg1 `functionName` arg2
   ```
   Example:
   ```haskell
   3 `add` 4  -- Same as add 3 4
   ```

4. **If-Then-Else**
   ```haskell
   if condition
     then expression1
     else expression2
   ```
   Example:
   ```haskell
   absolute x = if x < 0 then -x else x
   ```

5. **Let Expressions**
   ```haskell
   let variable = value in expression
   ```
   Example:
   ```haskell
   let square x = x * x in square 5  -- Returns 25
   ```

6. **Where Clauses**
   ```haskell
   expression
     where variable = value
   ```
   Example:
   ```haskell
   circleArea r = pi * rsquared
     where rsquared = r * r
   ```

### Comments

Haskell supports two types of comments:

1. **Single-line comments**
   Use `--` for comments that extend to the end of the line.
   ```haskell
   -- This is a single-line comment
   add x y = x + y  -- Adds two numbers
   ```

2. **Multi-line comments**
   Use `{-` to start and `-}` to end a multi-line comment.
   ```haskell
   {-
   This is a multi-line comment.
   It can span several lines.
   -}
   multiply x y = x * y
   ```

### Practical Tips

- Indentation is significant in Haskell. Use consistent spacing to group related code.
- Function and variable names start with a lowercase letter.
- Type names start with an uppercase letter.
- Use meaningful names for functions and variables to make your code self-documenting.
- Comments should explain why, not what. The code itself should be clear enough to understand what it does.

### Practice

Try writing these in a Haskell file:

1. A function to calculate the average of three numbers
2. A function using if-then-else to return the larger of two numbers
3. A multi-line comment explaining what your functions do

Remember, practice is key to becoming comfortable with Haskell's syntax!