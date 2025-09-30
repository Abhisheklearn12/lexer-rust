
# 🦀 lexer-rust

This project is a **handwritten lexer in Rust (`main.rs`)**, built as part of my journey into compiler construction.

I originally started this in **C**, as part of a custom compiler project (still in progress). Writing in C was challenging --> pointers, memory management, and debugging took a lot of time. To speed up learning, I reimplemented the lexer in **Rust**.

The result: a clean, safe, and fun lexer that runs under ~500 LOC with zero segfaults. 🚀

---
## ✨ Features

* ✅ **Single-file implementation** (`main.rs`)
* ✅ Tracks **line:col → line:col** spans for every token
* ✅ Supports:

  * Identifiers & keywords
  * Integers & floats (with scientific notation)
  * String literals with escape sequences (`\n`, `\t`, `\"`, etc.)
  * Line (`// ...`) & block (`/* ... */`) comments
  * Punctuation (`() {} [] , . ; :`)
  * Operators (`+ - * / % ^ ! = < > && || -> += -= ...`)
* ✅ Clear `LexError` reporting with spans
* ✅ Runs with `cargo run`
* ✅ Includes unit tests (`cargo test`)

---

## 📂 Project Layout

```
lexer-rust/
├── Cargo.toml
└── src/
    └── main.rs   # Entire lexer implementation
```

> Future: this can be refactored into multiple modules (`lexer.rs`, `token.rs`, `pos.rs`, `error.rs`) for a production-style structure.

---

## 🏗️ Architecture

```text
+-------------------+
|    Source Code    |
+-------------------+
         |
         v
+-------------------+
|   Lexer (main.rs) |
| - Iterates chars  |
| - Skips whitespace|
| - Handles comments|
| - Builds spans    |
+-------------------+
         |
         v
+-------------------+
|   Tokens          |
| - TokenKind enum  |
| - Token with span |
+-------------------+
         |
         v
+-------------------+
|   Errors          |
| - LexError type   |
| - Carries message |
| - Includes spans  |
+-------------------+
```

---

## 🚀 Getting Started

### Clone and Build

```bash
git clone <your-repo-url>
cd lexer-rust
cargo build
```

### Run the Demo

```bash
cargo run
```

Example output:

```
   1:1-1:3  Keyword(fn)
   1:4-1:7  Identifier(add)
   1:8-1:9  (
   1:10-1:11 Identifier(x)
   1:12-1:13 ,
   1:14-1:15 Identifier(y)
   ...
```

### Run Tests

```bash
cargo test
```

All tests should pass

---

## 💡 Why Rust after C?

When I began in **C**:

* Manual memory management slowed things down
* Pointer mistakes = segfaults everywhere
* Debugging took hours

When I switched to **Rust**:

* Pattern matching + enums = natural lexer design
* Borrow checker eliminated memory bugs
* Safer, clearer code in under ~700 LOC
* Development speed was much faster 🎉

This project is both **educational** and a **solid foundation** for further compiler development.

---

## 🔮 Future Roadmap

* Refactor into modules (`lexer.rs`, `token.rs`, `pos.rs`, `error.rs`), I didn't refactor only for learning purpose, but you can, it's easy.
* Add more keywords and operators
* Handle full C-like syntax
* Build a parser & AST generator
* Complete the **C compiler project** and compare Rust vs C

---

## 📖 Learning Journey

* 📌 Started with **C**, which gave me low-level insights into compiler design
* 📌 Reimplemented in **Rust**, which made development smoother and safer
* 📌 Next step: finish the **C compiler** and extend this Rust project into a full mini-compiler

---
Compiler is literally lots of fucking fascinating.
