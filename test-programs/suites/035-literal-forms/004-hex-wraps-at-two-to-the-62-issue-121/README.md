Pins a bug: a hexadecimal, binary or octal literal of 2^62 or more silently becomes another value, because the parser reads it through a 63-bit integer (issue #121).
