## Haskell Package Management

Haskell has two primary tools for package management: **Cabal** and **Stack**. Both tools help manage dependencies, build projects, and facilitate the development of Haskell applications. This guide will cover the basics of both tools, their usage, and key differences.

### Cabal

**Cabal** is a system for building and packaging Haskell libraries and applications. It includes a package manager (`cabal-install`) that allows you to install and manage Haskell packages.

#### Key Features of Cabal

- **Dependency Management**: Automatically resolves and installs dependencies from Hackage, the Haskell package repository.
- **Build Configuration**: Allows specifying build configurations and options in the `.cabal` file.
- **Package Installation**: Supports global and local package installations.

#### Getting Started with Cabal

1. **Install Cabal**: You can install Cabal using `ghcup` or your OS's package manager.

2. **Create a New Project**:

   ```bash
   mkdir myproject && cd myproject
   cabal init
   ```

3. **Add Dependencies**: Edit the generated `.cabal` file to add dependencies under the `build-depends` section.

4. **Build the Project**:

   ```bash
   cabal build
   ```

5. **Run the Project**:

   ```bash
   cabal run
   ```

6. **Install a Package**:

   ```bash
   cabal install package-name
   ```

### Stack

**Stack** is a more modern build tool that focuses on reproducibility and simplicity. It uses curated snapshots of package sets to ensure that builds are consistent across different environments.

#### Key Features of Stack

- **Reproducible Builds**: Ensures that builds are consistent by using snapshots.
- **Integrated Package Management**: Automatically manages dependencies and installs GHC versions.
- **Easy Project Setup**: Simplifies the process of creating and managing Haskell projects.

#### Getting Started with Stack

1. **Install Stack**: You can install Stack using the official installer or your OS's package manager.

2. **Create a New Project**:

   ```bash
   stack new myproject
   cd myproject
   ```

3. **Add Dependencies**: Edit the `package.yaml` file to add dependencies under the `dependencies` section.

4. **Build the Project**:

   ```bash
   stack build
   ```

5. **Run the Project**:

   ```bash
   stack exec myproject-exe
   ```

6. **Update Dependencies**:

   ```bash
   stack update
   ```

### Differences Between Cabal and Stack

- **Dependency Management**: Cabal resolves dependencies at build time, while Stack uses curated snapshots to ensure consistent builds.
- **Project Structure**: Stack encourages a specific project structure, while Cabal allows more flexibility.
- **Build Isolation**: Stack provides better isolation of dependencies, reducing conflicts between projects.

### Conclusion

Cabal and Stack are both powerful tools for managing Haskell packages and projects. Cabal is more traditional and flexible, while Stack emphasizes reproducibility and ease of use. Depending on your project needs and preferences, you can choose either tool to effectively manage your Haskell applications.

### Practice Exercises

1. Create a simple Haskell project using both Cabal and Stack, and compare the experience.
2. Experiment with adding dependencies to your project in both tools and observe how they handle version conflicts.
3. Explore the documentation for both Cabal and Stack to learn about advanced features like custom build configurations and testing.

By understanding and utilizing both Cabal and Stack, you can streamline your Haskell development process and manage your projects effectively!
