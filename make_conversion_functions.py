types = [
    ("uint8",  "uint8_t"),
    ("uint16", "uint16_t"),
    ("uint32", "uint32_t"),
    ("uint64", "uint64_t"),
    ("int8",   "int8_t"),
    ("int16",  "int16_t"),
    ("int32",  "int32_t"),
    ("int64",  "int64_t"),
    ("usize",  "size_t"),
    ("f",      "float"),
    ("d",      "double"),
]

print("\nDeclarations:\n")

for (source_type_abbrev, source_type) in types:
    for (target_type_abbrev, target_type) in types:
        code = f"extern {target_type} convert_{source_type_abbrev}_to_{target_type_abbrev}({source_type} value);"
        print(code)

print("\nBodies:\n")

for (source_type_abbrev, source_type) in types:
    for (target_type_abbrev, target_type) in types:
        code = f"{target_type} convert_{source_type_abbrev}_to_{target_type_abbrev}({source_type} value) {{ return ({target_type})(value); }}"
        print(code)
