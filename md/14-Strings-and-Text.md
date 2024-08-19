## Strings and Text Processing

In Haskell, strings are essentially lists of characters. This allows us to use many list functions for string processing.

### String Basics

1. Creating strings:
   ```haskell
   greeting = "Hello, World!"
   multiline = "This is a\nmultiline string"
   ```

2. String concatenation:
   ```haskell
   "Hello" ++ " " ++ "World"  -- "Hello World"
   ```

3. String as a list of characters:
   ```haskell
   ['H','e','l','l','o']  -- "Hello"
   ```

### Common String Functions

1. `length`: Get string length
   ```haskell
   length "Hello"  -- 5
   ```

2. `take` and `drop`: Substring operations
   ```haskell
   take 3 "Hello"  -- "Hel"
   drop 2 "Hello"  -- "llo"
   ```

3. `reverse`: Reverse a string
   ```haskell
   reverse "Hello"  -- "olleH"
   ```

4. `elem`: Check if a character is in a string
   ```haskell
   elem 'o' "Hello"  -- True
   ```

### Text Processing Functions

1. `words` and `unwords`: Split into words and join words
   ```haskell
   words "Hello World"  -- ["Hello","World"]
   unwords ["Hello","World"]  -- "Hello World"
   ```

2. `lines` and `unlines`: Split into lines and join lines
   ```haskell
   lines "Hello\nWorld"  -- ["Hello","World"]
   unlines ["Hello","World"]  -- "Hello\nWorld\n"
   ```

3. `toUpper` and `toLower` (from `Data.Char`):
   ```haskell
   import Data.Char (toUpper, toLower)
   map toUpper "Hello"  -- "HELLO"
   map toLower "WORLD"  -- "world"
   ```

### Practical Examples

1. Capitalize first letter:
   ```haskell
   capitalize [] = []
   capitalize (x:xs) = toUpper x : xs
   ```

2. Count occurrences of a character:
   ```haskell
   countChar c = length . filter (==c)
   ```

3. Palindrome check:
   ```haskell
   isPalindrome s = s == reverse s
   ```

### Working with Substrings

1. `isPrefixOf` and `isSuffixOf` (from `Data.List`):
   ```haskell
   import Data.List (isPrefixOf, isSuffixOf)
   "He" `isPrefixOf` "Hello"  -- True
   "ld" `isSuffixOf` "World"  -- True
   ```

2. `isInfixOf` (from `Data.List`):
   ```haskell
   import Data.List (isInfixOf)
   "ll" `isInfixOf` "Hello"  -- True
   ```

### String Interpolation (with `Text`)

For more complex string operations, consider using the `Text` type:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

name = "Alice"
age = 30
greeting = T.pack "Hello, " <> name <> T.pack ". You are " <> T.pack (show age) <> T.pack " years old."
```

### Practice Exercises

1. Write a function that counts the number of words in a string.
2. Create a function that censors a given word in a string by replacing it with asterisks.
3. Implement a simple Caesar cipher encryption/decryption function.

Remember, while Haskell's native string type is simple (just a list of characters), it may not be the most efficient for large-scale text processing. For performance-critical applications, consider using the `Text` or `ByteString` types from their respective libraries. Practice these string operations to become comfortable with text processing in Haskell!