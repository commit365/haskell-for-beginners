## Tuples and Pairs

Tuples are fixed-size collections that can hold elements of different types. Pairs are simply tuples with two elements.

### Creating Tuples

1. Pair (2-tuple):
   ```haskell
   point = (3, 4)
   person = ("Alice", 30)
   ```

2. Larger tuples:
   ```haskell
   triple = (1, "hello", True)
   quadruple = (2.5, 'a', [1,2,3], "world")
   ```

### Accessing Tuple Elements

1. Pattern matching:
   ```haskell
   (x, y) = (3, 4)
   -- x is 3, y is 4
   ```

2. Built-in functions for pairs:
   ```haskell
   fst (3, 4)  -- 3
   snd (3, 4)  -- 4
   ```

### Working with Tuples

1. Creating a list of tuples:
   ```haskell
   points = [(1,2), (3,4), (5,6)]
   ```

2. Tuple as function return:
   ```haskell
   divMod 7 3  -- (2, 1)
   ```

3. Swapping pair elements:
   ```haskell
   swap (x, y) = (y, x)
   ```

### Practical Examples

1. Representing a person's data:
   ```haskell
   type Person = (String, Int, String)  -- (Name, Age, City)
   alice :: Person
   alice = ("Alice", 30, "New York")
   ```

2. Function returning multiple values:
   ```haskell
   stats :: [Int] -> (Int, Int, Int)  -- (minimum, maximum, sum)
   stats xs = (minimum xs, maximum xs, sum xs)
   ```

3. Zipping lists:
   ```haskell
   zip [1,2,3] ['a','b','c']  -- [(1,'a'),(2,'b'),(3,'c')]
   ```

### Using Tuples in Functions

1. Pattern matching in function definition:
   ```haskell
   addPair :: (Int, Int) -> Int
   addPair (x, y) = x + y
   ```

2. Creating pairs from a list:
   ```haskell
   pairUp :: [a] -> [(a,a)]
   pairUp (x:y:rest) = (x,y) : pairUp rest
   pairUp _ = []
   ```

3. Unpacking tuples in list comprehensions:
   ```haskell
   sumPairs :: [(Int, Int)] -> [Int]
   sumPairs xs = [x + y | (x, y) <- xs]
   ```

### Tips and Tricks

1. Use tuples for heterogeneous collections, lists for homogeneous ones.
2. Prefer records (custom data types) over large tuples for better readability.
3. Remember that tuples of different sizes are different types.

### Practice Exercises

1. Write a function that takes a list of pairs and returns a pair of lists.
2. Create a function that calculates the distance between two points, represented as pairs.
3. Implement a function that groups a list into pairs, handling odd-length lists.

Remember, tuples are great for returning multiple values from a function or for representing simple structured data. However, for more complex structures, consider using custom data types. Practice using tuples in various scenarios to become comfortable with their syntax and use cases!