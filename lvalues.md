# Structure

An l-value is either:

1. A variable.
2. An l-value that is a public record followed by `.x`.
3. An l-value that is a pointer or writable reference to a public record followed by `->x`.
4. An l-value that is a heap array or writable reference to a heap array followed by `[n]`.

Validity:

1. Can't assing to an lvalue in the linear or type universes.
