# Obsi
Obsi is a minimal programming language designed for training large language models. It prioritizes simplicity and minimizes the number of tokens used, making it intentionally unreadable by humans.

## Features
- Lexer: Tokenizes input code.
- Parser: Generates an abstract syntax tree (AST) from tokenized code.

## Usage
Obsi provides several commands to interact with your code:

```
obsi --file <FILE> <COMMAND>
```

### Commands

- `lexer`- Print the tokens from the file.
- `parser`- Print the AST from the file.
- `compiler`- Produce a binary from the file (not yet implemented).
- `help`- Print this message or the help of the given subcommand(s).

### Options
- `-f, --file <FILE>` File to read.
- `-h, --help` Print help.
- `-V, --version` Print version.

## Example
Given the input:

```
@4 compute 4 a 4 b {
    4 five 
    4 c
    4 d
    five 5 
    c a + b 
    ? b < 5 {
        d a + 5
    } {
        d a - 5
    }
    ~ d
}

```
The parser will produce an AST representing the program structure. To understand what is the meaning of every token, you can read the lexer, and the parser. Don't worry they're simple.
