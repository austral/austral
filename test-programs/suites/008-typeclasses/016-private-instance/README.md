Test that a private instance of a foreign typeclass is not imported.

The typeclass `Foo` is public in the module `Foreign`, but the instance for `Bool`
is private and should not be usable.