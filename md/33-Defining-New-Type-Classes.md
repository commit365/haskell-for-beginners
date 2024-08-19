## Defining New Type Classes

In Haskell, you can create your own type classes to define a set of functions that can operate on different types. This allows you to create abstractions that can be implemented for various data types, enabling polymorphism and code reuse.

### Basic Syntax

The syntax for defining a new type class is as follows:

```haskell
class ClassName a where
    function1 :: a -> ReturnType1
    function2 :: a -> a -> ReturnType2
```

- `ClassName` is the name of the type class.
- `a` is a type variable that can represent any type.
- The functions defined in the type class specify the operations that must be implemented for any type that wants to be an instance of this class.

### Example: Defining a New Type Class

Let's define a simple type class called `Describable`, which will require types to implement a method that provides a description.

#### Step 1: Define the Type Class

```haskell
class Describable a where
    describe :: a -> String
```

### Step 2: Implementing Instances of the Type Class

Now, we can create instances of the `Describable` type class for different data types.

1. **Defining a Data Type**:
   ```haskell
   data Person = Person { name :: String, age :: Int }
   ```

2. **Making Person an Instance of Describable**:
   ```haskell
   instance Describable Person where
       describe (Person n a) = "This is " ++ n ++ ", age " ++ show a
   ```

3. **Defining Another Data Type**:
   ```haskell
   data Animal = Animal { species :: String, sound :: String }
   ```

4. **Making Animal an Instance of Describable**:
   ```haskell
   instance Describable Animal where
       describe (Animal s so) = "This is a " ++ s ++ " that says " ++ so
   ```

### Step 3: Using the Type Class

Now that we have defined the `Describable` type class and created instances for `Person` and `Animal`, we can use it in our code.

```haskell
main :: IO ()
main = do
    let alice = Person "Alice" 30
    let dog = Animal "Dog" "Woof"

    putStrLn (describe alice)  -- "This is Alice, age 30"
    putStrLn (describe dog)     -- "This is a Dog that says Woof"
```

### Example: Defining a Type Class for Area Calculation

Let's define another type class called `Area`, which will require types to implement a method for calculating the area.

#### Step 1: Define the Type Class

```haskell
class Area a where
    area :: a -> Float
```

#### Step 2: Implementing Instances of the Area Type Class

1. **Defining a Data Type for Circle**:
   ```haskell
   data Circle = Circle { radius :: Float }
   ```

2. **Making Circle an Instance of Area**:
   ```haskell
   instance Area Circle where
       area (Circle r) = pi * r * r
   ```

3. **Defining a Data Type for Rectangle**:
   ```haskell
   data Rectangle = Rectangle { width :: Float, height :: Float }
   ```

4. **Making Rectangle an Instance of Area**:
   ```haskell
   instance Area Rectangle where
       area (Rectangle w h) = w * h
   ```

### Step 3: Using the Area Type Class

Now we can calculate the area for different shapes using the `Area` type class.

```haskell
main :: IO ()
main = do
    let circle = Circle 5.0
    let rectangle = Rectangle 4.0 6.0

    putStrLn $ "Area of the circle: " ++ show (area circle)        -- Area of the circle: 78.53981633974483
    putStrLn $ "Area of the rectangle: " ++ show (area rectangle)  -- Area of the rectangle: 24.0
```

### Practice Exercises

1. Define a type class called `Comparable` with a method `compareTo` that compares two values and returns an `Ordering`. Implement this type class for `Int` and `String`.

2. Create a type class `JSONSerializable` with a method `toJSON` that converts a value to a JSON string. Implement this type class for a custom data type representing a book.

3. Define a type class `Convertible` with methods for converting between two types. Implement this type class for converting between Celsius and Fahrenheit.

### Tips

- Use descriptive names for your type classes to clarify their purpose.
- Keep your type class definitions focused on a specific behavior or capability.
- Remember that you can create multiple instances of a type class for different types, allowing for polymorphism.

Defining new type classes in Haskell allows you to create flexible and reusable abstractions. Practice creating and implementing your own type classes to become more comfortable with this powerful feature!