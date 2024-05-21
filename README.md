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

## Features
- [x] Ints
- [x] Booleans
- [ ] Strings
- [ ] Lists
- [x] Arithmetic Expressions
- [x] Equality Expressions
- [x] Grouping Expressions
- [ ] Comparison
- [x] If Expressions
- [x] Assignment
  - [x] Global Variables
  - [ ] Local Variables 
- [ ] Pattern Matching
- [x] Type Checking
- [x] Proper Error Handling and Reporting
  - [x] Syntax Errors
	- [x] Error Reporting
	- [x] Error Recovery
  - [x] Type Errors
    - [x] Error Reporting
- [ ] Module System/Imports
