## Variables and Bindings

In Haskell, variables are actually constants. Once a value is bound to a name, it doesn't change. This is part of Haskell's immutability principle.

### Basic Variable Binding

Use `=` to bind a value to a name:

```haskell
x = 5
message = "Hello, Haskell!"
```

### Let Expressions

Use `let` to create local bindings:

```haskell
let y = 10 in y * 2  -- Results in 20

let 
    a = 3
    b = 4
in a * b  -- Results in 12
```

### Where Clauses

Use `where` for bindings after the main expression:

```haskell
circleArea r = pi * rsquared
    where rsquared = r * r
```

### Function Parameters

Function parameters are also a form of binding:

```haskell
double x = x * 2
```

### Pattern Matching

Bind values using pattern matching:

```haskell
(x, y) = (1, 2)  -- x is bound to 1, y to 2

head:tail = [1, 2, 3]  -- head is 1, tail is [2, 3]
```

### Shadowing

Inner bindings can shadow outer ones:

```haskell
x = 5
doubleX = let x = 10 in x * 2  -- doubleX is 20, not 10
```

### Practical Examples

1. Using `let` in GHCi:
   ```haskell
   Prelude> let x = 5 in x + 3
   8
   ```

2. Function with where clause:
   ```haskell
   pythagoras a b = sqrt (aSquared + bSquared)
       where
           aSquared = a * a
           bSquared = b * b
   ```

3. Multiple bindings:
   ```haskell
   calculateStats numbers = (sum, average, length)
       where
           sum = foldr (+) 0 numbers
           length = foldr (\_ acc -> acc + 1) 0 numbers
           average = sum / fromIntegral length
   ```

### Important Points

- Variables in Haskell are immutable. Once bound, their value doesn't change.
- The scope of a binding is determined by where it's defined (`let`, `where`, or globally).
- Haskell uses lexical scoping, meaning inner bindings can shadow outer ones.

### Practice

1. Create a function that uses `let` to calculate the area of a rectangle.
2. Write a function using `where` to compute both the circumference and area of a circle.
3. Experiment with shadowing by creating nested `let` expressions with the same variable name.

Remember, understanding variables and bindings is crucial in Haskell, as they form the basis of how you structure and organize your code!