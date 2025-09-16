# Toy Imperative Language

Implang is a simple toy language for the purpose of learning LLVM.

## Language Features
### Types
- primitives: `int`, `bool`
- functional: `A -> B`
- structural: `(A, B, C)`

```go
(1, true) : (int, bool)
```

### Comments
Same as C.
- line comment: `// line comment`
- block comment: `/* block comment */`

### Declarations
```swift
let x = 1;     // immutable variable
var y = 2;     // mutable variable
var z: int;    // uninitialized mutable variable

// function declaration
func foo(x, y: int): int -> int { 
    ...
}
```

### Expressions
```swift
0 < (1 + 2 * 3)     // arithmetic/boolean
(x,y, z)            // tuple construction
m.0                 // tuple projection
x = v               // assignment
func(x) { ... }     // anonymous function
foo(x, y)           // function application
```

### Control Flow
```swift
// if-then-else
if (cond) {
    ...
} else {
    ...
}

// while-loop
while (cond) {
    ...
}

// for-loop
for (var i = 0; cond; i = i + 1) {
    ...
}

return v;   // return from function
continue;   // continue loop
break;      // break from loop
```

### Semantics
- Call by value
- Pass by value
- Closure capture by value