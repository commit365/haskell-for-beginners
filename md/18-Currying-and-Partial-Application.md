## Currying and Partial Application

Currying is the technique of converting a function that takes multiple arguments into a sequence of functions, each taking a single argument. Partial application is the process of fixing a number of arguments to a function, producing another function of smaller arity.

### Currying

In Haskell, all functions are automatically curried.

1. Basic concept:
   ```haskell
   -- This:
   add :: Int -> Int -> Int
   add x y = x + y

   -- Is equivalent to:
   add :: Int -> (Int -> Int)
   add x = \y -> x + y
   ```

2. Curried function application:
   ```haskell
   add 3 4        -- 7
   (add 3) 4      -- 7 (explicit parentheses)
   ```

### Partial Application

Partial application is creating a new function by fixing some arguments of an existing function.

1. Simple partial application:
   ```haskell
   add5 = add 5   -- add5 is a new function that adds 5 to its argument
   add5 3         -- 8
   ```

2. With standard functions:
   ```haskell
   map (2*) [1,2,3]   -- [2,4,6]
   filter (>5) [1..10]  -- [6,7,8,9,10]
   ```

### Practical Examples

1. Creating specialized functions:
   ```haskell
   multiply :: Int -> Int -> Int
   multiply x y = x * y

   double = multiply 2
   triple = multiply 3

   double 4  -- 8
   triple 4  -- 12
   ```

2. Partial application with higher-order functions:
   ```haskell
   applyTwice :: (a -> a) -> a -> a
   applyTwice f x = f (f x)

   applyTwice (add 3) 7  -- 13
   ```

3. Building complex functions:
   ```haskell
   addAndMultiply :: Int -> Int -> Int -> Int
   addAndMultiply x y z = (x + y) * z

   addFiveThenDouble = addAndMultiply 2 3
   addFiveThenDouble 4  -- 20
   ```

### Advanced Usage

1. Sections (partial application of infix operators):
   ```haskell
   halvesLessThan = (<) . (/2)
   halvesLessThan 10 6  -- True (because 6/2 < 10)
   ```

2. Flip function for changing argument order:
   ```haskell
   flip :: (a -> b -> c) -> b -> a -> c
   flip f x y = f y x

   divideBy = flip (/)
   divideBy 2 10  -- 5 (equivalent to 10 / 2)
   ```

3. Partial application in list operations:
   ```haskell
   map (2^) [1..5]  -- [2,4,8,16,32]
   filter ((==3) . length) ["cat", "dog", "mouse"]  -- ["cat", "dog"]
   ```

### Practice Exercises

1. Write a function `addPairs` that adds pairs of numbers, then use partial application to create a function that adds 5 to each element of a pair.

2. Create a function `between` that checks if a number is between two other numbers, then use partial application to create functions `isAdult` and `isChild` that check if an age is in the adult or child range.

3. Implement a curried function `applyOperation` that takes an operator and two numbers, then use partial application to create functions for addition, subtraction, multiplication, and division.

### Tips

- Always consider the order of arguments in your functions to make partial application more useful.
- Use type signatures to help understand and work with curried functions.
- Remember that in Haskell, `f x y` is the same as `(f x) y` due to left associativity.

Currying and partial application are fundamental concepts in Haskell that enable powerful function composition and reuse. Practice these techniques to write more concise and flexible code!