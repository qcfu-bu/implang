- [x] Invalidate `continue/break` commands inside closures inside loops.  
```
while (true) {
    func foo(x) {
        break; \\ Scope error!
    }
}
```

- [x] Parse repeated function applications.
```
(foo(a,b,c))(x, y, z) // Current
foo(a,b,c)(x,y,z)     // Intended
```

- [x] Fix global variable shadowing issue.
```
var x: bool = true;
var x: int;
var x: int = 123;
```