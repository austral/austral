A `Buffer` is a heap-allocated, arbitrarily large array. It is transparently
resized as elements are inserted or removed.

# API

```
-- Create
Create(): Buffer[T]

forall T: Free
Initialize(size: Size, elem: T): Buffer[T]

-- Query
Length(buf: Buffer[T] @ Rho): Size

-- Retrieval
Nth(buf: Buffer[T] @ Rho, index: Size): T @ Rho'
Remove_Nth(buf: Buffer[T] @! Rho, index: Size): T

-- Insertion
Push(buf: Buffer[T] @! Rho, elem: T): Unit;
Pop(buf: Buffer[T] @! Rho): Option[T];

-- Destruction
Dispose(buf: Buffer[T]);
```
