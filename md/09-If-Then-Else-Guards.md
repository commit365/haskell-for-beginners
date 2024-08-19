## If-Then-Else and Guards

Haskell provides two main ways to implement conditional logic: If-Then-Else statements and Guards. Both are used to execute different code based on conditions.

### If-Then-Else

The basic syntax is:

```haskell
if condition
  then expression1
  else expression2
```

Examples:

1. Simple if-then-else:
   ```haskell
   absoluteValue x = if x < 0
                       then -x
                       else x
   ```

2. Nested if-then-else:
   ```haskell
   grade score = if score >= 90
                   then "A"
                   else if score >= 80
                     then "B"
                     else if score >= 70
                       then "C"
                       else "F"
   ```

3. If-then-else as an expression:
   ```haskell
   max2 a b = if a > b then a else b
   ```

### Guards

Guards provide a cleaner way to express multiple conditions. The syntax is:

```haskell
functionName args
  | condition1 = result1
  | condition2 = result2
  ...
  | otherwise  = defaultResult
```

Examples:

1. Simple guard:
   ```haskell
   absoluteValue x
     | x < 0     = -x
     | otherwise = x
   ```

2. Multiple conditions:
   ```haskell
   grade score
     | score >= 90 = "A"
     | score >= 80 = "B"
     | score >= 70 = "C"
     | otherwise   = "F"
   ```

3. Guards with multiple parameters:
   ```haskell
   compare2 x y
     | x > y     = "Greater"
     | x < y     = "Less"
     | otherwise = "Equal"
   ```

### Practical Examples

1. BMI calculator using if-then-else:
   ```haskell
   bmiTell weight height =
     if bmi <= 18.5
       then "Underweight"
       else if bmi <= 25.0
         then "Normal"
         else "Overweight"
     where bmi = weight / height ^ 2
   ```

2. Day of week using guards:
   ```haskell
   dayName day
     | day == 1  = "Monday"
     | day == 2  = "Tuesday"
     | day == 3  = "Wednesday"
     | day == 4  = "Thursday"
     | day == 5  = "Friday"
     | day == 6  = "Saturday"
     | day == 7  = "Sunday"
     | otherwise = "Invalid day"
   ```

### Practice

1. Write a function using if-then-else to determine if a year is a leap year.
2. Create a function using guards to classify a triangle as equilateral, isosceles, or scalene based on its side lengths.
3. Implement a simple calculator function using guards that performs different operations (+, -, *, /) based on an input operator.

Remember, while if-then-else is familiar from many languages, guards often lead to more readable code in Haskell, especially when dealing with multiple conditions. Practice using both to get a feel for when each is most appropriate!