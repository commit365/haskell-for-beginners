## Creating a Simple Game in Haskell

In this guide, we will create a simple text-based number guessing game in Haskell. This project will help you understand basic Haskell concepts such as input/output, random number generation, and control flow.

### Step 1: Setting Up Your Project

1. **Create a New Haskell Project**: If you are using Stack, create a new project:

   ```bash
   stack new number-guessing-game
   cd number-guessing-game
   ```

2. **Open the Main File**: Open the `app/Main.hs` file in your favorite text editor.

### Step 2: Import Necessary Modules

At the top of your `Main.hs` file, import the necessary modules:

```haskell
import System.Random (randomRIO)
import Control.Monad (when)
import Text.Read (readMaybe)
```

### Step 3: Define the Game Logic

Now, let's define the main game logic. We will create a function that generates a random number and allows the player to guess it.

```haskell
-- Function to play the guessing game
playGame :: Int -> IO ()
playGame secretNumber = do
    putStrLn "Guess the number (between 1 and 100):"
    guessInput <- getLine

    case readMaybe guessInput :: Maybe Int of
        Just guess -> do
            if guess < secretNumber
                then do
                    putStrLn "Too low! Try again."
                    playGame secretNumber
                else if guess > secretNumber
                    then do
                        putStrLn "Too high! Try again."
                        playGame secretNumber
                    else
                        putStrLn "Congratulations! You've guessed the number!"
        Nothing -> do
            putStrLn "Please enter a valid number."
            playGame secretNumber
```

### Step 4: Generate a Random Number

Next, we will create a function to generate a random number between 1 and 100 and start the game:

```haskell
-- Function to start the game
startGame :: IO ()
startGame = do
    secretNumber <- randomRIO (1, 100)  -- Generate a random number
    putStrLn "Welcome to the Number Guessing Game!"
    playGame secretNumber
```

### Step 5: Main Function

Finally, we will define the `main` function to run the game:

```haskell
-- Main function
main :: IO ()
main = startGame
```

### Step 6: Running the Game

1. **Compile and Run**: Compile and run your Haskell application:

   ```bash
   stack run
   ```

2. **Play the Game**: Follow the prompts to guess the number. The game will provide feedback on whether your guess is too low, too high, or correct.

### Example Output

```
Welcome to the Number Guessing Game!
Guess the number (between 1 and 100):
50
Too low! Try again.
Guess the number (between 1 and 100):
75
Too high! Try again.
Guess the number (between 1 and 100):
62
Congratulations! You've guessed the number!
```

### Conclusion

You have successfully created a simple number guessing game in Haskell! This project demonstrates basic Haskell concepts such as input/output, random number generation, and control flow.

### Further Enhancements

- **Add a Score System**: Track the number of guesses the player makes and display a score at the end.
- **Replay Option**: Allow the player to play multiple rounds without restarting the program.
- **Difficulty Levels**: Implement different difficulty levels that change the range of numbers.

### Practice Exercises

1. Modify the game to provide hints (e.g., "You're getting warmer") based on the player's previous guesses.
2. Create a graphical version of the game using a library like `Gloss` or `SDL`.
3. Implement a leaderboard that saves the best scores to a file.

By completing this project, you have gained practical experience in Haskell programming while creating an engaging application. Enjoy experimenting with further enhancements!