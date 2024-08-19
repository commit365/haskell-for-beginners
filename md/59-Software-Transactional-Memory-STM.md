## Software Transactional Memory (STM)

**Software Transactional Memory (STM)** is a concurrency control mechanism that simplifies the management of shared mutable state in Haskell. It allows you to perform multiple state-changing operations atomically, meaning that the operations either all succeed or none do, which helps prevent race conditions and inconsistencies in concurrent programs.

### Key Concepts of STM

1. **Atomicity**: STM ensures that a block of operations is executed as a single atomic transaction. If any part of the transaction fails, the entire transaction is rolled back.

2. **TVar**: The primary mutable variable in STM is `TVar`, which is a transactional variable that holds a value and can be read or written within a transaction.

3. **Transactions**: You can group operations on `TVar`s into transactions using the `atomically` function. This function executes the transaction and ensures that it is atomic.

### Importing STM Module

To use STM in Haskell, you need to import the `Control.Concurrent.STM` module:

```haskell
import Control.Concurrent.STM
```

### Basic Operations with STM

Here are some basic operations you can perform with `TVar` and STM:

1. **Creating a TVar**: Use `newTVar` to create a new `TVar`.

2. **Reading a TVar**: Use `readTVar` to read the value from a `TVar`.

3. **Writing to a TVar**: Use `writeTVar` to update the value of a `TVar`.

4. **Using atomically**: Wrap your operations in `atomically` to ensure they are executed as a transaction.

### Example: Bank Account Transfer

Let’s implement a simple example of transferring money between two bank accounts using STM.

#### Step 1: Define the Account Type

```haskell
type Account = TVar Float

-- Create a new account with a given balance
openAccount :: Float -> STM Account
openAccount balance = newTVar balance
```

#### Step 2: Implement Transfer Function

```haskell
transfer :: Account -> Account -> Float -> STM ()
transfer from to amount = do
    -- Read the balances
    fromBalance <- readTVar from
    toBalance <- readTVar to
    
    -- Check if the transfer is valid
    if fromBalance >= amount
        then do
            -- Perform the transfer
            writeTVar from (fromBalance - amount)
            writeTVar to (toBalance + amount)
        else
            error "Insufficient funds"
```

#### Step 3: Main Function to Execute Transactions

```haskell
main :: IO ()
main = do
    -- Create two accounts
    accountA <- atomically (openAccount 100)
    accountB <- atomically (openAccount 50)

    -- Perform a transfer
    atomically $ transfer accountA accountB 30

    -- Check balances after transfer
    balanceA <- atomically $ readTVar accountA
    balanceB <- atomically $ readTVar accountB

    putStrLn $ "Balance of Account A: " ++ show balanceA  -- Output: 70.0
    putStrLn $ "Balance of Account B: " ++ show balanceB  -- Output: 80.0
```

### Example Output

When you run the above program, you will see:

```
Balance of Account A: 70.0
Balance of Account B: 80.0
```

### Advantages of STM

1. **Simplicity**: STM provides a simpler and more composable way to manage shared state compared to traditional locking mechanisms.

2. **Avoids Deadlocks**: Since STM uses optimistic concurrency control, it avoids the common pitfalls of deadlocks associated with locks.

3. **Composability**: You can easily compose multiple STM operations into a single transaction, enhancing code modularity and readability.

### Summary

- **STM**: A concurrency control mechanism that allows for atomic transactions on mutable state.
- **TVar**: The primary mutable variable used in STM, which can be read and written within transactions.
- **Atomic Transactions**: Group operations using `atomically` to ensure they are executed as a single atomic operation.

### Practice Exercises

1. Implement a function that allows multiple transfers between accounts and ensures that all operations are atomic.
2. Create a simple banking application that allows users to deposit and withdraw money using STM.
3. Extend the example to handle multiple threads performing transfers concurrently, ensuring thread safety.

### Conclusion

Software Transactional Memory (STM) in Haskell provides a powerful and elegant way to manage concurrency and shared state. By using STM, you can write concurrent programs that are simpler, safer, and more maintainable. Practice these concepts to become proficient in using STM in Haskell!
