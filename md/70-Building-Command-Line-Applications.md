## Building Command-Line Applications in Haskell

Haskell provides several libraries to facilitate the creation of command-line applications. This guide will cover how to set up a basic command-line application using the `optparse-applicative` library, which allows you to define command-line options and parse them easily.

### Setting Up Your Project

1. **Create a New Haskell Project**: If you are using Stack, you can create a new project with the following command:

   ```bash
   stack new my-cli-app
   cd my-cli-app
   ```

2. **Add Dependencies**: Open your `.cabal` file or `package.yaml` and add `optparse-applicative` to the dependencies:

   ```yaml
   dependencies:
     - base >=4.7 && <5
     - optparse-applicative >=0.15 && <0.17
   ```

3. **Install Dependencies**: If you are using Stack, run:

   ```bash
   stack build
   ```

### Importing Necessary Modules

In your main Haskell file (e.g., `Main.hs`), import the required modules:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Data.Semigroup ((<>))
import System.Exit (exitFailure)
```

### Defining Command-Line Options

You can define your command-line options using the `Options.Applicative` module. Here’s an example of how to set up a simple command-line application that takes a name and an age as input:

```haskell
data Options = Options
    { name :: String
    , age  :: Int
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        ( long "name"
       <> metavar "NAME"
       <> help "Your name" )
    <*> option auto
        ( long "age"
       <> metavar "AGE"
       <> help "Your age" )

main :: IO ()
main = do
    opts <- execParser optsParser
    putStrLn $ "Hello, " ++ name opts ++ "! You are " ++ show (age opts) ++ " years old."
  where
    optsParser = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Greet a user with NAME and AGE"
     <> header "A simple command-line application in Haskell" )
```

### Running Your Application

1. **Compile the Application**: If you are using Stack, run:

   ```bash
   stack build
   ```

2. **Run the Application**: You can run your application from the command line and provide the necessary arguments:

   ```bash
   stack exec my-cli-app -- --name Alice --age 30
   ```

### Example Output

When you run the application with the above command, you should see:

```
Hello, Alice! You are 30 years old.
```

### Handling Errors

You can also handle errors gracefully by checking the validity of the input. For example, you can add validation to ensure that the age is a positive integer:

```haskell
import Control.Monad (when)

main :: IO ()
main = do
    opts <- execParser optsParser
    when (age opts < 0) $ do
        putStrLn "Error: Age must be a non-negative integer."
        exitFailure
    putStrLn $ "Hello, " ++ name opts ++ "! You are " ++ show (age opts) ++ " years old."
```

### Summary

- **optparse-applicative**: A powerful library for parsing command-line options in Haskell.
- **Defining Options**: Use the `Parser` type to define command-line options and their descriptions.
- **Running the Application**: Compile and run your application, passing the necessary arguments.
- **Error Handling**: Implement error handling to validate user input.

### Practice Exercises

1. Extend the command-line application to include additional options, such as a greeting message.
2. Implement a command that calculates the year of birth based on the provided age.
3. Create a command-line application that reads from a file and processes its contents based on user input.

### Conclusion

Building command-line applications in Haskell is straightforward with the `optparse-applicative` library. By following this guide, you can create a simple command-line tool that accepts user input and performs actions based on that input. Practice these concepts to become proficient in building command-line applications in Haskell!