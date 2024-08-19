## Type Class Instances

Type class instances in Haskell allow you to define how specific types implement the functions specified by a type class. This enables polymorphism, allowing functions to operate on different types while adhering to a common interface.

### Defining a Type Class

Before creating instances, you first need to define a type class. Here’s a quick recap of how to define a type class:

```haskell
class ClassName a where
    function1 :: a -> ReturnType1
    function2 :: a -> a -> ReturnType2
```

### Creating Type Class Instances

To create an instance of a type class for a specific type, you use the `instance` keyword followed by the type class name and the type you are implementing it for.

#### Example: Defining a Type Class

Let's define a type class called `Describable`, which requires a method `describe`.

```haskell
class Describable a where
    describe :: a -> String
```

### Example of Creating Instances

1. **Defining a Data Type**:
   ```haskell
   data Person = Person { name :: String, age :: Int }
   ```

2. **Creating an Instance for Person**:
   ```haskell
   instance Describable Person where
       describe (Person n a) = "This is " ++ n ++ ", age " ++ show a
   ```

3. **Defining Another Data Type**:
   ```haskell
   data Animal = Animal { species :: String, sound :: String }
   ```

4. **Creating an Instance for Animal**:
   ```haskell
   instance Describable Animal where
       describe (Animal s so) = "This is a " ++ s ++ " that says " ++ so
   ```

### Using Type Class Instances

Once you have defined instances for your types, you can use the functions defined in the type class.

```haskell
main :: IO ()
main = do
    let alice = Person "Alice" 30
    let dog = Animal "Dog" "Woof"

    putStrLn (describe alice)  -- "This is Alice, age 30"
    putStrLn (describe dog)     -- "This is a Dog that says Woof"
```

### Example: Multiple Instances for Different Types

You can create multiple instances of the same type class for different types. For example, let's define a type class `Area` for calculating the area of different shapes.

#### Step 1: Define the Area Type Class

```haskell
class Area a where
    area :: a -> Float
```

#### Step 2: Implementing Instances

1. **Circle Data Type**:
   ```haskell
   data Circle = Circle { radius :: Float }

   instance Area Circle where
       area (Circle r) = pi * r * r
   ```

2. **Rectangle Data Type**:
   ```haskell
   data Rectangle = Rectangle { width :: Float, height :: Float }

   instance Area Rectangle where
       area (Rectangle w h) = w * h
   ```

### Using the Area Instances

Now you can calculate the area for different shapes using the `Area` type class.

```haskell
main :: IO ()
main = do
    let circle = Circle 5.0
    let rectangle = Rectangle 4.0 6.0

    putStrLn $ "Area of the circle: " ++ show (area circle)        -- Area of the circle: 78.53981633974483
    putStrLn $ "Area of the rectangle: " ++ show (area rectangle)  -- Area of the rectangle: 24.0
```

### Instance Deriving

Haskell provides a way to automatically derive instances for certain type classes. For example, you can derive instances for `Eq`, `Ord`, `Show`, and `Read` using the `deriving` keyword.

#### Example of Deriving Instances

```haskell
data Point = Point { x :: Float, y :: Float } deriving (Show, Eq, Ord)

-- Now you can use the derived instances
main :: IO ()
main = do
    let p1 = Point 1.0 2.0
    let p2 = Point 3.0 4.0

    print (p1 == p2)  -- False
    print (show p1)   -- "Point {x = 1.0, y = 2.0}"
```

### Practice Exercises

1. Define a type class called `Convertible` with a method `convert` that converts between two types. Implement this type class for converting between Celsius and Fahrenheit.

2. Create a type class `Printable` that requires a `printDetails` method. Implement this type class for a custom data type representing a book.

3. Define a type class `Comparable` with a method `compareTo`. Implement this type class for `Int` and `String`.

### Conclusion

Type class instances allow you to define how specific types implement the functions specified by a type class, enabling polymorphism and code reuse. Practice creating and using type class instances to become more proficient in Haskell programming!