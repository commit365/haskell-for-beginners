## Unit Testing with HUnit

**HUnit** is a unit testing framework for Haskell that allows you to write and run tests for your Haskell code. It provides a simple way to define test cases and organize them into test suites.

### Setting Up HUnit

To get started with HUnit, you need to install the `HUnit` library. You can do this using the Cabal package manager. If you are using a project with a `.cabal` file, add `HUnit` to the `build-depends` section:

```cabal
build-depends: base >=4.7 && <5, HUnit
```

If you are using Stack, add `HUnit` to your `stack.yaml` file under the `extra-deps` section or include it in your `package.yaml`.

### Importing HUnit

To use HUnit in your test files, import the necessary modules:

```haskell
import Test.HUnit
```

### Writing Test Cases

You can define test cases using the `TestCase` constructor. Each test case consists of an assertion that checks if a value matches an expected result.

#### Example: Simple Unit Test

Let’s write a simple unit test for a function that adds two numbers.

1. **Define the Function**:

```haskell
-- A simple function to add two numbers
add :: Int -> Int -> Int
add x y = x + y
```

2. **Write the Test Cases**:

```haskell
-- Define test cases
testAdd :: Test
testAdd = TestCase (do
    assertEqual "1 + 1 should be 2" 2 (add 1 1)
    assertEqual "2 + 3 should be 5" 5 (add 2 3)
    assertEqual "0 + 0 should be 0" 0 (add 0 0)
    assertEqual "(-1) + 1 should be 0" 0 (add (-1) 1)
    )
```

### Organizing Tests into a Test Suite

You can organize multiple test cases into a test suite using the `TestList` constructor.

```haskell
-- Define the test suite
tests :: Test
tests = TestList [testAdd]
```

### Running the Tests

To run the tests, you can use the `runTestTT` function. Here’s how to set up the `main` function to execute the tests:

```haskell
main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts > 0
        then putStrLn "Some tests failed."
        else putStrLn "All tests passed."
```

### Complete Example

Here’s the complete code for the example above:

```haskell
import Test.HUnit

-- A simple function to add two numbers
add :: Int -> Int -> Int
add x y = x + y

-- Define test cases
testAdd :: Test
testAdd = TestCase (do
    assertEqual "1 + 1 should be 2" 2 (add 1 1)
    assertEqual "2 + 3 should be 5" 5 (add 2 3)
    assertEqual "0 + 0 should be 0" 0 (add 0 0)
    assertEqual "(-1) + 1 should be 0" 0 (add (-1) 1)
    )

-- Define the test suite
tests :: Test
tests = TestList [testAdd]

-- Main function to run tests
main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts > 0
        then putStrLn "Some tests failed."
        else putStrLn "All tests passed."
```

### Running the Example

To run the tests, compile the program and execute it. You should see output indicating whether the tests passed or failed.

### Example Output

```
Cases: 4  Tried: 4  Errors: 0  Failures: 0
All tests passed.
```

### Summary

- **HUnit**: A unit testing framework for Haskell that allows you to define and run tests.
- **Test Cases**: Use `TestCase` to define assertions and check expected results.
- **Test Suites**: Organize multiple test cases using `TestList`.
- **Running Tests**: Use `runTestTT` to execute the tests and check results.

### Practice Exercises

1. Write unit tests for a function that calculates the factorial of a number.
2. Create tests for a function that determines if a number is prime.
3. Implement tests for a string manipulation function, such as reversing a string.

### Conclusion

Unit testing with HUnit in Haskell provides a straightforward way to ensure the correctness of your code. By writing tests, you can catch errors early and maintain the reliability of your programs as they evolve. Practice these concepts to become proficient in unit testing with HUnit!