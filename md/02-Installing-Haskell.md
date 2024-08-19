## Installing Haskell and Setting Up Your Development Environment

Setting up Haskell is straightforward. We'll use GHCup, a universal installer for Haskell tools.

### Step 1: Install GHCup

1. **Windows**: 
   - Open PowerShell and run:
     ```
     Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true
     ```

2. **macOS/Linux**:
   - Open Terminal and run:
     ```
     curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
     ```

3. Follow the prompts to complete the installation.

### Step 2: Verify Installation

Open a new terminal window and run:
```
ghc --version
ghci --version
cabal --version
```
You should see version information for each tool.

### Step 3: Choose a Text Editor or IDE

Pick one:
1. **Visual Studio Code**:
   - Install VS Code
   - Add the "Haskell" extension

2. **IntelliJ IDEA with IntelliJ-Haskell**:
   - Install IntelliJ IDEA
   - Add the "IntelliJ-Haskell" plugin

3. **Vim/Emacs**: If you prefer, use your favorite text editor with Haskell syntax highlighting.

### Step 4: Create Your First Haskell Program

1. Create a file named `hello.hs`
2. Add this code:
   ```haskell
   main :: IO ()
   main = putStrLn "Hello, Haskell!"
   ```
3. In the terminal, navigate to the file's directory and run:
   ```
   ghc hello.hs
   ./hello
   ```

You should see "Hello, Haskell!" printed.

### Troubleshooting

- If you encounter issues, check the official Haskell documentation or ask for help in the Haskell community forums.

You're now ready to start coding in Haskell!