## Lambda Expressions and Anonymous Functions

Lambda expressions, also known as anonymous functions, are functions without a name. They're useful for creating quick, one-off functions, especially when passing functions as arguments.

### Basic Syntax

The basic syntax for a lambda expression is:

```haskell
\parameter1 parameter2 ... -> expression
```

The backslash `\` is used to denote the start of a lambda expression, mimicking the λ symbol from lambda calculus.

### Simple Examples

1. A lambda that adds two numbers:
   ```haskell
   (\x y -> x + y) 3 4  -- 7
   ```

2. Using a lambda with `map`:
   ```haskell
   map (\x -> x * 2) [1,2,3,4]  -- [2,4,6,8]
   ```

3. Lambda with pattern matching:
   ```haskell
   (\(x,y) -> x + y) (3,4)  -- 7
   ```

### Practical Uses

1. As function arguments:
   ```haskell
   filter (\x -> x > 5) [1..10]  -- [6,7,8,9,10]
   ```

2. In list comprehensions:
   ```haskell
   [(x,y) | x <- [1..3], y <- [1..3], (\x y -> x + y) x y < 5]
   -- [(1,1),(1,2),(1,3),(2,1),(2,2),(3,1)]
   ```

3. Creating closures:
   ```haskell
   let multiplyBy = \n -> (\x -> n * x)
   let double = multiplyBy 2
   double 5  -- 10
   ```

### Comparison with Named Functions

Lambda expressions can often replace simple named functions:

1. Named function:
   ```haskell
   addOne x = x + 1
   map addOne [1,2,3]  -- [2,3,4]
   ```

2. Equivalent lambda:
   ```haskell
   map (\x -> x + 1) [1,2,3]  -- [2,3,4]
   ```

### Multi-line Lambdas

For more complex lambdas, you can use parentheses and layout:

```haskell
(\x y -> 
    let sum = x + y
        product = x * y
    in sum + product) 3 4  -- 19
```

### Partial Application with Lambdas

Lambdas can be partially applied:

```haskell
let addTo = \x -> (\y -> x + y)
let addFive = addTo 5
addFive 3  -- 8
```

### Capturing Variables

Lambdas can capture variables from their enclosing scope:

```haskell
let factor = 2
let scale = map (\x -> factor * x)
scale [1,2,3]  -- [2,4,6]
```

### Practice Exercises

1. Write a lambda expression that takes a list and returns the sum of the squares of its elements.

2. Use a lambda with `filter` to keep only the even-length strings from a list of strings.

3. Create a lambda that takes two parameters and returns a tuple of their sum and difference.

### Tips

- Use lambdas for short, simple functions, especially when passing functions as arguments.
- For more complex functions, consider using named functions for better readability.
- Remember that `(\x -> ...)` is equivalent to `\x -> ...` when there's only one parameter.

Remember, while lambda expressions are powerful and convenient, they should be used judiciously. For complex or reusable functions, named functions are often more readable and maintainable. Practice using lambdas in various scenarios to become comfortable with their syntax and use cases!