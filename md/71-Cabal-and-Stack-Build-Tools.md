## Cabal and Stack Build Tools

### Overview of Cabal

**Cabal** is a system for building and packaging Haskell libraries and applications. It provides a common interface for package authors and distributors, allowing them to build and manage Haskell projects in a portable way. The main components of Cabal include:

- **Cabal Files**: These files (`.cabal`) describe the package, including its dependencies, modules, and other metadata.
- **cabal-install**: The command-line interface for managing Haskell packages, fetching dependencies, and building projects.

#### Key Features of Cabal:

- **Dependency Management**: Automatically resolves and installs dependencies from Hackage.
- **Build Configuration**: Allows specifying build configurations and options in the `.cabal` file.
- **Multi-Project Support**: Supports building multiple packages in a single project.

### Getting Started with Cabal

1. **Installing Cabal**: You can install Cabal using `ghcup`, your OS's package manager, or by downloading the installer from the [Cabal website](https://www.haskell.org/cabal/).

2. **Creating a New Project**:

   ```bash
   mkdir myproject && cd myproject
   cabal init
   ```

   This command will create a new `.cabal` file and the necessary directory structure.

3. **Building the Project**:

   ```bash
   cabal build
   ```

4. **Running the Project**:

   ```bash
   cabal run
   ```

5. **Updating Dependencies**:

   ```bash
   cabal update
   ```

### Overview of Stack

**Stack** is another build tool for Haskell that focuses on reproducibility and simplicity. It manages project dependencies and provides a curated set of packages to ensure compatibility.

#### Key Features of Stack:

- **Reproducible Builds**: Ensures that builds are consistent across different environments by using snapshots.
- **Integrated Package Management**: Automatically manages dependencies and installs GHC versions.
- **Easy Project Setup**: Simplifies the process of creating and managing Haskell projects.

### Getting Started with Stack

1. **Installing Stack**: You can install Stack using the official installer or your OS's package manager. For example, on macOS, you can use Homebrew:

   ```bash
   brew install haskell-stack
   ```

2. **Creating a New Project**:

   ```bash
   stack new myproject
   cd myproject
   ```

3. **Building the Project**:

   ```bash
   stack build
   ```

4. **Running the Project**:

   ```bash
   stack exec myproject-exe
   ```

5. **Updating Dependencies**:

   ```bash
   stack update
   ```

### Differences Between Cabal and Stack

- **Dependency Management**: Cabal resolves dependencies at build time, while Stack uses curated snapshots to ensure consistent builds.
- **Project Structure**: Stack encourages a specific project structure, while Cabal allows more flexibility.
- **Build Isolation**: Stack provides better isolation of dependencies, reducing conflicts between projects.

### Summary

- **Cabal**: A versatile tool for building and packaging Haskell applications, ideal for managing libraries and applications with complex dependencies.
- **Stack**: A build tool that emphasizes reproducibility and simplicity, making it easier to manage Haskell projects with consistent builds.

### Practice Exercises

1. Create a simple Haskell project using both Cabal and Stack, and compare the experience.
2. Experiment with adding dependencies to your project in both tools and observe how they handle version conflicts.
3. Explore the documentation for both Cabal and Stack to learn about advanced features like custom build configurations and testing.

### Conclusion

Cabal and Stack are powerful tools for managing Haskell projects. By understanding how to use these tools effectively, you can streamline your development process and ensure that your Haskell applications are built and managed efficiently. Practice these concepts to become proficient in using Cabal and Stack for Haskell development!
