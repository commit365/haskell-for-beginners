## Generalized Algebraic Data Types (GADTs)

**GADTs** are an extension of regular algebraic data types in Haskell that allow you to specify the return type of constructors more precisely. This enables you to encode richer type information and constraints directly into the type system, leading to more expressive and type-safe code.

### Enabling GADTs

To use GADTs in your Haskell code, you need to enable the `GADTs` language extension by adding the following pragma at the top of your Haskell file:

```haskell
{-# LANGUAGE GADTs #-}
```

### Defining GADTs

The syntax for defining a GADT is slightly different from that of regular data types. In a GADT, you can specify the type of each constructor explicitly.

#### Example: A Simple GADT

Here’s a basic example of a GADT that represents a type-safe expression language:

```haskell
data Expr a where
    Lit  :: Int -> Expr Int
    BoolLit :: Bool -> Expr Bool
    Add  :: Expr Int -> Expr Int -> Expr Int
    If   :: Expr Bool -> Expr a -> Expr a -> Expr a
```

In this example:
- `Lit` constructs an expression of type `Expr Int`.
- `BoolLit` constructs an expression of type `Expr Bool`.
- `Add` takes two `Expr Int` and returns an `Expr Int`.
- `If` takes a condition of type `Expr Bool` and two branches of type `Expr a`, returning an expression of the same type as the branches.

### Using GADTs

You can write functions that pattern match on GADTs to extract values and perform computations based on their types.

#### Example: Evaluating Expressions

Here’s a function that evaluates the expressions defined above:

```haskell
eval :: Expr a -> a
eval (Lit x)      = x
eval (BoolLit b)  = b
eval (Add x y)    = eval x + eval y
eval (If cond t e) = if eval cond then eval t else eval e
```

### Example Usage

Now, let’s see how you can use the `Expr` GADT and the `eval` function:

```haskell
main :: IO ()
main = do
    let expr1 = Add (Lit 3) (Lit 5)  -- Represents the expression 3 + 5
    let expr2 = If (BoolLit True) (Lit 10) (Lit 20)  -- Represents if True then 10 else 20
    
    print $ eval expr1  -- Output: 8
    print $ eval expr2  -- Output: 10
```

### Advantages of GADTs

1. **Type Safety**: GADTs allow you to encode more information in the type system, reducing the chances of runtime errors.
  
2. **Expressiveness**: You can define more complex data structures and relationships, such as type-safe heterogeneous lists or expression languages.

3. **Pattern Matching**: When pattern matching on GADTs, the type checker can infer the type of the expression being matched, allowing for more precise type handling.

### Summary

- **GADTs**: Generalized Algebraic Data Types allow you to specify the return type of constructors explicitly, enhancing type safety and expressiveness.
- **Defining GADTs**: Use the `where` clause to define constructors with specific return types.
- **Using GADTs**: Pattern match on GADTs to extract values and perform computations based on their types.

### Practice Exercises

1. Define a GADT for a simple type-safe calculator that can handle addition, subtraction, and multiplication of integers.
2. Create a GADT that represents a type-safe list where each element can be of a different type but adheres to a specific interface (e.g., all elements can be shown).
3. Implement a GADT-based representation for a simple state machine, where each state can have different types of transitions.

### Conclusion

GADTs provide a powerful way to enhance the expressiveness of Haskell's type system, allowing for more complex and type-safe data representations. By understanding and utilizing GADTs, you can write more robust and flexible Haskell programs. Practice these concepts to become proficient in using GADTs!
