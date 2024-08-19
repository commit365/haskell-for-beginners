## List Comprehensions

List comprehensions are a concise and powerful way to create lists based on existing lists or ranges. They provide a clear, readable syntax for generating new lists by applying operations to elements of other lists.

### Basic Syntax

```haskell
[expression | generator, conditions]
```

Where:
- `expression` is what you want in the new list
- `generator` defines where to get values from
- `conditions` (optional) filter the values

### Simple Examples

1. Squaring numbers:
   ```haskell
   squares = [x^2 | x <- [1..10]]
   -- [1,4,9,16,25,36,49,64,81,100]
   ```

2. Even numbers:
   ```haskell
   evens = [x | x <- [1..20], even x]
   -- [2,4,6,8,10,12,14,16,18,20]
   ```

3. Combining lists:
   ```haskell
   pairs = [(x,y) | x <- [1,2,3], y <- ['a','b']]
   -- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
   ```

### More Complex Examples

1. Pythagorean triples:
   ```haskell
   pythTriples = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
   -- [(3,4,5),(6,8,10)]
   ```

2. Custom length strings:
   ```haskell
   lengths = [(w, length w) | w <- ["hello", "world", "!"], length w > 1]
   -- [("hello",5),("world",5)]
   ```

3. Nested list comprehension:
   ```haskell
   matrix = [[i*j | j <- [1..3]] | i <- [1..3]]
   -- [[1,2,3],[2,4,6],[3,6,9]]
   ```

### Practical Applications

1. Filtering a list of tuples:
   ```haskell
   getAdults = [name | (name, age) <- [("Alice", 25), ("Bob", 17), ("Charlie", 30)], age >= 18]
   -- ["Alice","Charlie"]
   ```

2. Creating a multiplication table:
   ```haskell
   multTable n = [[i*j | j <- [1..n]] | i <- [1..n]]
   ```

3. Finding common elements:
   ```haskell
   commonElements xs ys = [x | x <- xs, elem x ys]
   ```

### Tips and Tricks

1. Use `_` for unused variables:
   ```haskell
   repeatThrice x = [x | _ <- [1..3]]
   ```

2. Combine multiple generators:
   ```haskell
   [(x,y) | x <- [1..3], y <- [x..3]]
   -- [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
   ```

3. Use `let` for local definitions:
   ```haskell
   [let square x = x*x in (x, square x) | x <- [1..5]]
   -- [(1,1),(2,4),(3,9),(4,16),(5,25)]
   ```

### Practice Exercises

1. Write a list comprehension to generate all possible combinations of two dice rolls.
2. Create a function that uses a list comprehension to find all factors of a given number.
3. Implement a list comprehension that generates a list of coordinates for a chess board (A1 to H8).

Remember, list comprehensions are a powerful tool in Haskell for creating and manipulating lists. They can often replace loops and filter/map combinations found in other languages, leading to more concise and readable code. Practice using them to solve various problems and you'll quickly see their benefits!