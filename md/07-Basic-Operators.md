## Basic Operators: Arithmetic, Logical, Comparison

Haskell provides a variety of operators for arithmetic, logical operations, and comparisons. Let's explore the most common ones:

### Arithmetic Operators

1. Addition: `+`
2. Subtraction: `-`
3. Multiplication: `*`
4. Division: `/`
5. Integer division: `div`
6. Modulus (remainder): `mod` or `%`
7. Exponentiation: `^`

Examples:
```haskell
5 + 3   -- 8
10 - 4  -- 6
2 * 6   -- 12
15 / 2  -- 7.5
15 `div` 2  -- 7 (integer division)
15 `mod` 4  -- 3 (remainder)
2 ^ 3   -- 8 (2 to the power of 3)
```

### Logical Operators

1. AND: `&&`
2. OR: `||`
3. NOT: `not`

Examples:
```haskell
True && False  -- False
True || False  -- True
not True       -- False
```

### Comparison Operators

1. Equal to: `==`
2. Not equal to: `/=`
3. Greater than: `>`
4. Less than: `<`
5. Greater than or equal to: `>=`
6. Less than or equal to: `<=`

Examples:
```haskell
5 == 5   -- True
5 /= 4   -- True
7 > 3    -- True
2 < 1    -- False
5 >= 5   -- True
6 <= 10  -- True
```

### Operator Precedence

Operators have different precedence levels. Use parentheses when in doubt:

```haskell
2 + 3 * 4     -- 14 (multiplication before addition)
(2 + 3) * 4   -- 20 (addition first due to parentheses)
```

### Function Application

Function application has higher precedence than all operators:

```haskell
sqrt 4 + 2  -- 4.0 (not sqrt of 6)
sqrt (4 + 2)  -- 2.449489742783178
```

### Practical Examples

1. Calculate the average of three numbers:
   ```haskell
   average3 a b c = (a + b + c) / 3
   ```

2. Check if a number is even:
   ```haskell
   isEven n = n `mod` 2 == 0
   ```

3. Determine if a year is a leap year:
   ```haskell
   isLeapYear year = (year `mod` 4 == 0) && (year `mod` 100 /= 0 || year `mod` 400 == 0)
   ```

### Practice

1. Write a function to calculate the area of a triangle given its base and height.
2. Create a function that returns True if a number is both greater than 10 and even.
3. Implement a simple calculator function that takes an operator (+, -, *, /) and two numbers, and returns the result.

Remember, these operators are the building blocks for more complex expressions and functions in Haskell. Practice combining them in different ways to solve various problems!