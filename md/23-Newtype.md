## Newtype

`newtype` is a keyword in Haskell that allows you to create a new type that is distinct from an existing type but has the same underlying representation. This is useful for improving type safety and code clarity without incurring runtime overhead.

### Basic Syntax

The syntax for defining a new type is:

```haskell
newtype NewTypeName = Constructor ExistingType
```

### Creating a Newtype

1. **Defining a Newtype**:
   ```haskell
   newtype UserId = MkUserId Int
   ```

2. **Using the Newtype**:
   ```haskell
   myUserId :: UserId
   myUserId = MkUserId 42
   ```

### Benefits of Using Newtype

1. **Type Safety**:
   By wrapping a type in a `newtype`, you create a distinct type that prevents mixing it up with other types, even if they have the same underlying representation.
   ```haskell
   newtype OrderId = MkOrderId Int
   ```

   This ensures that you cannot accidentally use a `UserId` where an `OrderId` is expected.

2. **Improved Readability**:
   Using `newtype` can make your code more expressive by providing meaningful names for types.
   ```haskell
   newtype Email = MkEmail String
   ```

3. **No Runtime Overhead**:
   `newtype` is optimized by the compiler to have no runtime overhead compared to using the underlying type directly.

### Pattern Matching with Newtype

You can use pattern matching to extract the underlying value from a `newtype`.

```haskell
getUserId :: UserId -> Int
getUserId (MkUserId id) = id
```

### Practical Examples

1. **Defining a Newtype for a Specific Purpose**:
   ```haskell
   newtype Name = MkName String

   greet :: Name -> String
   greet (MkName n) = "Hello, " ++ n
   ```

2. **Using Newtype with Type Classes**:
   You can define instances for type classes for your newtype.
   ```haskell
   newtype Celsius = MkCelsius Float

   instance Show Celsius where
       show (MkCelsius temp) = show temp ++ " °C"

   -- Usage
   temp :: Celsius
   temp = MkCelsius 25.0
   ```

3. **Combining with Other Types**:
   ```haskell
   data User = User { userId :: UserId, userName :: String }

   newtype UserId = MkUserId Int

   createUser :: Int -> String -> User
   createUser uid name = User (MkUserId uid) name
   ```

### When to Use Newtype

- **When you want to create a distinct type for clarity**: If you have multiple types that are represented by the same underlying type (e.g., `Int` for IDs), use `newtype` to distinguish them.
  
- **When you want to enforce type safety**: Using `newtype` can help prevent errors by ensuring that functions expecting a specific type cannot accept another type with the same underlying representation.

### Practice Exercises

1. Define a `newtype` for `ISBN` that wraps a `String`. Create a function that formats the ISBN for display.

2. Create a `newtype` for `Username` that wraps a `String`, and implement a function that checks if a given string is a valid username (e.g., non-empty and alphanumeric).

3. Implement a `newtype` for `Temperature` that wraps a `Float`, and write functions to convert between Celsius and Fahrenheit.

### Tips

- Use `newtype` when you want to create a new type that has a single constructor and a single field.
- Remember that `newtype` is a compile-time construct and does not add any runtime overhead.
- Utilize `newtype` to improve the expressiveness of your code and avoid type-related errors.

Using `newtype` effectively can enhance the safety and clarity of your Haskell code. Practice creating and using `newtype` to become comfortable with this powerful feature!
