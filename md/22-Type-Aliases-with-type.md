## Type Aliases with `type`

Type aliases in Haskell allow you to create a new name for an existing type. This can make your code more readable and easier to maintain, especially when dealing with complex types.

### Basic Syntax

The syntax for creating a type alias is:

```haskell
type AliasName = ExistingType
```

### Creating Type Aliases

1. **Simple Type Alias**:
   You can create a type alias for a basic type:
   ```haskell
   type Age = Int
   ```

2. **Using the Type Alias**:
   ```haskell
   myAge :: Age
   myAge = 30
   ```

3. **Type Alias for a Complex Type**:
   You can also create aliases for more complex types, such as tuples or lists:
   ```haskell
   type Point = (Float, Float)
   ```

4. **Using the Complex Type Alias**:
   ```haskell
   myPoint :: Point
   myPoint = (3.0, 4.0)
   ```

### Type Aliases for Function Types

Type aliases can also be useful for simplifying function signatures.

1. **Function Type Alias**:
   ```haskell
   type Transformer a = a -> a
   ```

2. **Using the Function Type Alias**:
   ```haskell
   double :: Transformer Int
   double x = x * 2

   toUpperCase :: Transformer String
   toUpperCase str = map toUpper str
   ```

### Practical Examples

1. **Defining a Type Alias for a Record**:
   You can create a type alias for a record type:
   ```haskell
   type Person = (String, Int)  -- (Name, Age)

   getName :: Person -> String
   getName (name, _) = name
   ```

2. **Using Type Aliases in Data Structures**:
   ```haskell
   type Student = (String, Int, [String])  -- (Name, Age, Subjects)

   studentInfo :: Student
   studentInfo = ("Alice", 20, ["Math", "Science"])
   ```

3. **Defining a Type Alias for a List of Tuples**:
   ```haskell
   type CoordinateList = [(Float, Float)]

   myCoordinates :: CoordinateList
   myCoordinates = [(1.0, 2.0), (3.0, 4.0)]
   ```

### Benefits of Using Type Aliases

1. **Improved Readability**:
   Type aliases can make complex types easier to understand at a glance.
   ```haskell
   type UserID = Int
   type UserDatabase = [(UserID, String)]  -- List of (UserID, Username)
   ```

2. **Simplifying Function Signatures**:
   Using type aliases can reduce clutter in function signatures.
   ```haskell
   type StringTransformer = String -> String

   transform :: StringTransformer
   transform str = reverse str
   ```

3. **Easier Refactoring**:
   If you need to change the underlying type, you only need to update the alias definition.
   ```haskell
   type Email = String  -- If you later want to change this to a new type, just update here.
   ```

### Limitations

- Type aliases do not create new types; they are merely alternative names for existing types. This means that type aliases do not enforce any additional type safety.
- Aliases are not recursive. You cannot define a type alias that refers to itself directly.

### Practice Exercises

1. Create a type alias for a `Book` that includes the title (String), author (String), and year published (Int). Write a function that takes a `Book` and returns a formatted string.

2. Define a type alias for a list of integers and write a function that calculates the average of the integers in the list.

3. Create a type alias for a function that takes two integers and returns a Boolean value indicating if the first is greater than the second.

### Tips

- Use type aliases to clarify the purpose of complex types in your code.
- Keep your type alias names descriptive to enhance code readability and maintainability.
- Remember that type aliases do not create new types, so they do not provide additional type safety.

Type aliases are a powerful feature in Haskell that can enhance the clarity and maintainability of your code. Practice creating and using type aliases to become more comfortable with this aspect of Haskell programming!