# rust-shell

A simple shell implemented in Rust following the [Codecrafters Build Your Own Shell](https://codecrafters.io) project.

## Overview

**rust-shell** is designed to help you understand how operating systems interpret commands. This project covers:
- Parsing user input
- Handling built-in commands
- Process creation and management
- Simple I/O redirection

The goal is to provide a minimal yet functional shell that you can further extend or experiment with.

## Features

- **Basic Command Parsing:** Tokenizes input strings and executes commands.
- **Built-In Commands:** Implements internal commands like `cd`, `exit`, etc.
- **Process Handling:** Forks and executes external commands using Rust’s standard library.
- **Error Handling:** Gracefully handles errors to avoid shell crashes.

## Getting Started

### Prerequisites

- [Rust](https://www.rust-lang.org/tools/install) (version 1.50 or later is recommended)
- A Unix-like operating system (Linux, macOS)

### Installation

1. **Clone the repository:**

   ```bash
   git clone https://github.com/Strife-01/rust-shell.git
   cd rust-shell
   ```
2. **Build the project using Cargo:**

   ```bash
   cargo build --release
   ```
3. **Run the shell:**

   ```bash
   cargo run --release
   ```

### Usage

Once running, you can use the shell much like any other Unix shell:

- **Executing commands:** Type any valid Unix command and press Enter.
- **Built-in commands:** Use `cd` to change directories or `exit` to close the shell.
- **Customization:** Feel free to modify the source code to add more functionality or improve performance.

### Project Structure
```bash
rust-shell/
├── Cargo.toml         # Project configuration file
└── src/
    └── main.rs        # Entry point for the shell application
```

## Contributing

Contributions are welcome! If you find a bug or have a feature request, please open an issue. Pull requests are also appreciated.

- Fork the repository.
- Create your feature branch:  
  ```bash
  git checkout -b feature/my-new-feature
  ```
- Commit your changes:
  ```bash
  git commit -am 'Add some feature'
  ```
- Push to the branch:
  ```bash
  git push origin feature/my-new-feature
  ```

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Codecrafters](https://codecrafters.io) for the original "Build Your Own Shell" project concept.

Feel free to let me know if you need further adjustments!
