## Lists: Creation, Manipulation, and Functions

Lists are a fundamental data structure in Haskell. They are homogeneous (all elements must be of the same type) and immutable.

### List Creation

1. Direct creation:
   ```haskell
   numbers = [1, 2, 3, 4, 5]
   chars = ['a', 'b', 'c']
   ```

2. Range notation:
   ```haskell
   oneToTen = [1..10]
   evenNumbers = [2,4..20]
   lettersAtoE = ['a'..'e']
   ```

3. List comprehension:
   ```haskell
   squares = [x^2 | x <- [1..5]]
   ```

### List Manipulation

1. Concatenation (`++`):
   ```haskell
   [1,2,3] ++ [4,5]  -- [1,2,3,4,5]
   ```

2. Cons operator (`:`)
   ```haskell
   1 : [2,3,4]  -- [1,2,3,4]
   ```

3. Head and Tail:
   ```haskell
   head [1,2,3]  -- 1
   tail [1,2,3]  -- [2,3]
   ```

4. Init and Last:
   ```haskell
   init [1,2,3]  -- [1,2]
   last [1,2,3]  -- 3
   ```

### Common List Functions

1. `length`: Get list length
   ```haskell
   length [1,2,3,4]  -- 4
   ```

2. `sum` and `product`: Sum or multiply all elements
   ```haskell
   sum [1,2,3,4]     -- 10
   product [1,2,3,4] -- 24
   ```

3. `reverse`: Reverse a list
   ```haskell
   reverse [1,2,3]  -- [3,2,1]
   ```

4. `take` and `drop`: Take or drop n elements
   ```haskell
   take 3 [1,2,3,4,5]  -- [1,2,3]
   drop 3 [1,2,3,4,5]  -- [4,5]
   ```

5. `elem`: Check if element exists in list
   ```haskell
   elem 3 [1,2,3,4]  -- True
   ```

6. `map`: Apply function to all elements
   ```haskell
   map (*2) [1,2,3]  -- [2,4,6]
   ```

7. `filter`: Keep elements that satisfy a predicate
   ```haskell
   filter even [1,2,3,4,5]  -- [2,4]
   ```

### Practical Examples

1. Generate a list of squares:
   ```haskell
   squares n = [x^2 | x <- [1..n]]
   ```

2. Remove duplicates from a list:
   ```haskell
   removeDuplicates [] = []
   removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
   ```

3. Zip two lists together:
   ```haskell
   customZip [] _ = []
   customZip _ [] = []
   customZip (x:xs) (y:ys) = (x,y) : customZip xs ys
   ```

### Practice

1. Write a function that returns the last element of a list safely (handling empty lists).
2. Create a function that takes a list of numbers and returns a list of their absolute values.
3. Implement a function that finds the maximum element in a list using recursion.

Remember, lists are central to Haskell programming. They're used extensively and understanding how to work with them is crucial. Practice creating and manipulating lists, and using list functions to solve various problems!