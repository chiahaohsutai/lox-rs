# Lox-rs

Implementation of JLox from Robert Nystrom's book [Crafting Interpreters](https://craftinginterpreters.com/) using Rust. This projects follows each chapter in the book but instead of using Java, it implements JLox using Rust instead. The project also focuses on using Rust patterns instead of common programming patterns that belong to Java.

## What's JLox?

JLox is a Java-based interpreter for the Lox programming language, developed in the first part of Robert Nystrom’s book Crafting Interpreters. This interpreter is implemented as a tree-walk interpreter, meaning it parses the source code into an abstract syntax tree (AST) and then directly interprets this tree to execute the program. Key feayures include:
Key features include:
- Variables and Scope: JLox allows the declaration of variables using the var keyword, supporting both global and local scopes. Variables can be assigned and reassigned, and their scope is determined by their placement within code blocks.
- Control Flow Constructs: It includes standard control flow mechanisms such as if statements, while loops, and for loops, enabling complex logical structures and iterative processes.
- Functions: JLox supports first-class functions, allowing the definition of reusable code blocks with the fun keyword. Functions can accept parameters, return values, and be passed as arguments to other functions.
- Closures: Functions in JLox can capture and retain access to variables from their defining environment, facilitating the creation of closures for advanced functional programming techniques.
- Classes and Objects: As an object-oriented language, JLox enables the definition of classes with fields and methods, supporting encapsulation and the creation of complex data structures.
- Inheritance: JLox allows classes to inherit from other classes, promoting code reuse and the establishment of hierarchical relationships between classes.
- Dynamic Typing: The language is dynamically typed, meaning variable types are determined at runtime, providing flexibility in coding but requiring careful type management by the programmer.
- Error Handling: JLox includes mechanisms for error detection and reporting, aiding in debugging and ensuring robust code execution.
