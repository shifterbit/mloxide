# mloxide (WIP)
An ML derivative written in rust.

## Building and Running
```bash
git clone https://github.com/shifterbit/mloxide.git
cd mloxide
cargo build
./target/debug/mloxide <source-file>
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

## TODO
- [x] Ints
- [x] Booleans
- [ ] Strings
- [ ] Lists
- [x] Arithmetic Expressions
- [x] Equality Expressions
- [x] Grouping Expressions
- [ ] Comparison
- [x] If Expressions
- [ ] Assignment
- [ ] Pattern Matching
- [x] Type Checking
- [ ] Proper Error Handling and Reporting
  - [ ] Syntax Errors
	- [x] Error Reporting
	- [ ] Error Recovery
  - [ ] Type Errors
    - [ ] Error Reporting
- [ ] Module System/Imports
