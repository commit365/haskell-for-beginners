## Let and Where Clauses

Let and Where clauses are used to create local bindings in Haskell. They help make code more readable and avoid repeating complex expressions.

### Let Clauses

`let` introduces local bindings inside an expression.

Syntax:
```haskell
let bindings in expression
```

Examples:

1. Simple let:
   ```haskell
   squareSum x y = 
     let square a = a * a
     in square x + square y
   ```

2. Multiple bindings:
   ```haskell
   cylinderSurfaceArea radius height = 
     let circleArea = pi * radius ^ 2
         sideArea = 2 * pi * radius * height
     in 2 * circleArea + sideArea
   ```

3. In list comprehensions:
   ```haskell
   [ let square x = x * x in (x, square x) | x <- [1..5] ]
   -- [(1,1),(2,4),(3,9),(4,16),(5,25)]
   ```

### Where Clauses

`where` introduces local bindings after the main expression.

Syntax:
```haskell
expression
  where bindings
```

Examples:

1. Simple where:
   ```haskell
   squareSum x y = square x + square y
     where square a = a * a
   ```

2. Multiple bindings:
   ```haskell
   cylinderSurfaceArea radius height = 2 * circleArea + sideArea
     where
       circleArea = pi * radius ^ 2
       sideArea = 2 * pi * radius * height
   ```

3. With pattern matching:
   ```haskell
   head_tail (x:xs) = "Head: " ++ show x ++ ", Tail: " ++ show xs
     where (x:xs) = [1,2,3,4,5]
   ```

### Practical Examples

1. BMI calculator with where:
   ```haskell
   bmiTell weight height
     | bmi <= 18.5 = "Underweight"
     | bmi <= 25.0 = "Normal"
     | otherwise   = "Overweight"
     where bmi = weight / height ^ 2
   ```

2. Quadratic formula with let:
   ```haskell
   quadratic a b c = 
     let discriminant = b^2 - 4*a*c
         root1 = (-b + sqrt discriminant) / (2*a)
         root2 = (-b - sqrt discriminant) / (2*a)
     in (root1, root2)
   ```

### Let vs Where

- `let` is an expression (can be used anywhere an expression is allowed)
- `where` is a declaration (tied to a specific function or pattern match)
- `let` bindings are available only in the following expression
- `where` bindings are available throughout the entire function definition

### Practice

1. Rewrite the `cylinderSurfaceArea` function using `let` instead of `where`.
2. Create a function to calculate the perimeter and area of a rectangle, using `where` to define helper calculations.
3. Write a function that uses `let` to create a local function, then uses that function in a list comprehension.

Remember, both `let` and `where` help in organizing code and avoiding repetition. `let` is more flexible in terms of where it can be used, while `where` often leads to cleaner function definitions. Practice using both to get a feel for when each is most appropriate!