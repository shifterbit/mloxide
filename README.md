# mloxide (WIP)
An ML derivative written in rust.

## Building and Running
### Clone the Repository
```bash
git clone https://github.com/shifterbit/mloxide.git
cd mloxide
```

### Building with Cargo
```bash
cargo build --release # omit --release for debug build
./target/release/mloxide <path to source fille> # ./target/debug/mloxide for debug builds
```

### Building with Nix
[Install Nix](https://nixos.org/download/)

[Make sure flakes are enabled](https://nixos.wiki/wiki/Flakes)

```bash
nix build
./result/bin/mloxide <path to source file>
```

## Example Code
### Equality Checks
```sml
2 + 2 == 4
```
### If Expressions
```sml
if (2 + 2) == 4 
then 2 
else 9
```
### Variables
```sml
val x = 2;
val y = 10;
let
  val z = y + 1;
in if z <= x
   then x + y
   else y + z
```

## Features
- [x] Ints
- [x] Booleans
- [ ] Strings
- [ ] Lists
- [x] Tuples
- [x] Let Expressions
- [x] Arithmetic Expressions
- [x] Equality Expressions
- [x] Grouping Expressions
- [x] Comparison
- [x] If Expressions
- [x] Assignment
  - [x] Global Variables
  - [x] Local Variables 
- [ ] Pattern Matching
  - [x] Wildcard/Variable Matching
  - [ ] Matching Literal Values
  - [ ] Matching Constructors
  - [ ] Nested Patterns
- [x] Type Checking
- [x] Type System
  - [x] Type Inference
  - [ ] Sum Types
  - [ ] Type Declarations
- [x] Proper Error Handling and Reporting
  - [x] Syntax Errors
	- [x] Error Reporting
	- [x] Error Recovery
  - [x] Type Errors
    - [x] Error Reporting
- [ ] Module System/Imports
