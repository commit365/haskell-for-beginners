## Functional Dependencies

**Functional dependencies** (often abbreviated as **fundeps**) are a feature of Haskell's type system that allow you to specify relationships between the parameters of multi-parameter type classes. They provide a way to express that one or more type parameters uniquely determine another type parameter, which helps the type checker resolve ambiguities in type inference.

### Enabling Functional Dependencies

To use functional dependencies in your Haskell code, you need to enable the `FunctionalDependencies` language extension. This is typically done by adding the following pragma at the top of your Haskell file:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
```

### Defining Functional Dependencies

Functional dependencies are specified in the class declaration using a vertical bar (`|`). The syntax is as follows:

```haskell
class ClassName a b | a -> b where
    method :: a -> b
```

In this example, `a -> b` indicates that for each unique type `a`, there is exactly one corresponding type `b`. This means that knowing `a` allows you to uniquely determine `b`.

### Example: A Simple Use Case

Let's create a simple example to illustrate functional dependencies. We will define a class for a simple arithmetic operation that depends on two types.

#### Step 1: Define the Class with Functional Dependencies

```haskell
class Additive a b | a -> b where
    add :: a -> a -> b
```

In this example, `Additive` is a type class that defines an `add` method. The functional dependency `a -> b` indicates that the type `a` uniquely determines the type `b`.

#### Step 2: Create Instances of the Class

Now, we can create instances of this class for specific types:

```haskell
instance Additive Int Int where
    add x y = x + y

instance Additive Float Float where
    add x y = x + y
```

#### Step 3: Using the Class

You can now use the `Additive` class in your code:

```haskell
main :: IO ()
main = do
    let intResult = add (5 :: Int) (10 :: Int)
    print intResult  -- Output: 15

    let floatResult = add (5.5 :: Float) (10.5 :: Float)
    print floatResult  -- Output: 16.0
```

### Benefits of Functional Dependencies

1. **Type Safety**: Functional dependencies allow the compiler to resolve types more accurately, reducing ambiguity and potential runtime errors.

2. **Improved Type Inference**: By specifying dependencies, you can help the type checker infer types more effectively, especially in complex type class hierarchies.

3. **Enhanced Expressiveness**: Functional dependencies enable you to express relationships between types that are not possible with regular multi-parameter type classes.

### Limitations of Functional Dependencies

While functional dependencies are powerful, they come with some limitations:

1. **Complexity**: They can make type class definitions more complex and harder to read, especially in larger codebases.

2. **Ambiguity in Instances**: If not used carefully, you can still encounter ambiguities, especially when multiple instances might apply.

3. **Not Part of Haskell 98**: Functional dependencies are not part of the Haskell 98 standard, which may limit portability to some older Haskell implementations.

### Summary

- **Functional Dependencies**: Allow you to specify relationships between type parameters in multi-parameter type classes, enhancing type safety and inference.
- **Syntax**: Use the vertical bar (`|`) in class declarations to indicate dependencies.
- **Example**: Illustrated with a simple `Additive` class that demonstrates how to define and use functional dependencies.

### Practice Exercises

1. Define a type class for a `Show`-like functionality that uses functional dependencies to determine the output type based on the input type.
2. Create a class for a `Convertible` type that allows conversion between different types, using functional dependencies to specify the relationship.
3. Implement a type class for geometric shapes that determines the area based on the shape type, using functional dependencies to link the shape to its area type.

### Conclusion

Functional dependencies in Haskell provide a powerful mechanism for expressing relationships between type parameters in multi-parameter type classes. By understanding and utilizing functional dependencies, you can write more robust, type-safe, and expressive Haskell code. Practice these concepts to become proficient in using functional dependencies!
