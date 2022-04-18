#!/usr/bin/env python3


class ExhaustivenessError(Exception):
    pass


class Type(object):
    pass


class IntegerType(Type):
    def __init__(self, signed: bool, width: int):
        assert isinstance(signed, bool)
        self.signed = signed
        assert isinstance(width, int)
        assert width in [8, 16, 32, 64]
        self.width = width

    def __eq__(self, other) -> bool:
        return (
            isinstance(other, IntegerType)
            and (self.signed == other.signed)
            and (self.width == other.width)
        )

    def is_int(self) -> bool:
        return True

    def is_float(self) -> bool:
        return False

    def name(self) -> str:
        s: str = "int" if self.signed else "nat"
        w: str = str(self.width)
        return f"au_{s}{w}_t"

    def austral_name(self) -> str:
        s: str = "Int" if self.signed else "Nat"
        w: str = str(self.width)
        return f"{s}{w}"

    def abbrev(self) -> str:
        s: str = "int" if self.signed else "nat"
        w: str = str(self.width)
        return f"{s}{w}"

    def max(self) -> str:
        """
        The name of the constant that holds the largest value for this integer type.
        """
        s: str = "INT" if self.signed else "UINT"
        w: str = str(self.width)
        return f"{s}{w}_MAX"


class FloatType(Type):
    def __init__(self, width: int):
        assert isinstance(width, int)
        assert width in [32, 64]
        self.width = width

    def __eq__(self, other) -> bool:
        return (
            isinstance(other, FloatType)
            and
            (self.width == other.width)
        )

    def is_int(self) -> bool:
        return False

    def is_float(self) -> bool:
        return True

    def name(self) -> str:
        if self.width == 32:
            return "float"
        elif self.width == 64:
            return "double"
        else:
            raise ExhaustivenessError()

    def austral_name(self) -> str:
        if self.width == 32:
            return "Float32"
        elif self.width == 64:
            return "Float64"
        else:
            raise ExhaustivenessError()

    def abbrev(self) -> str:
        return self.name()

TYPES: list[Type] = [
    # Nat8
    IntegerType(signed=False, width=8),
    # Int8
    IntegerType(signed=True,  width=8),

    # Nat16
    IntegerType(signed=False, width=16),
    # Int16
    IntegerType(signed=True,  width=16),

    # Nat32
    IntegerType(signed=False, width=32),
    # Int32
    IntegerType(signed=True,  width=32),

    # Nat64
    IntegerType(signed=False, width=64),
    # Int64
    IntegerType(signed=True,  width=64),

    # Float32
    FloatType(width=32),
    # Float64
    FloatType(width=64),
]

def make_declarations():
    for source in TYPES:
        for target in TYPES:
            make_decl(source, target)

def make_decl(source: Type, target: Type):
    name: str = f"convert_{source.abbrev()}_to_{target.abbrev()}"
    decl: str = f"extern {target.name()} {name}({source.name()} value);"
    print(decl)

def make_bodies():
    for source in TYPES:
        for target in TYPES:
            # Make the function signature:
            fn_name: str = f"convert_{source.abbrev()}_to_{target.abbrev()}"
            sig: str     = f"{target.name()} {fn_name}({source.name()} value) {{"
            print(sig)
            # Write checks specific to the kind of conversion.
            src_int: bool = source.is_int()
            src_flt: bool = source.is_float()
            tar_int: bool = target.is_int()
            tar_flt: bool = target.is_float()
            if (src_int and tar_int):
                # Converting an integer to an integer
                make_int_conversion(source, target)
            elif (src_int and tar_flt):
                # Converting an integer to a float
                make_int_to_float_conversion(source, target)
            elif (src_flt and tar_int):
                # Converting a float to an integer
                make_float_to_int_conversion(source, target)
            elif (src_flt and tar_flt):
                # Converting a float into a float
                make_float_conversion(source, target)
            else:
                raise ExhaustivenessError()
            # Perform the conversion
            print(f"  return ({target.name()})(value);")
            # Print the closing brace.
            print("}\n\n")

def make_int_conversion(source: IntegerType, target: IntegerType):
    # The prefix for error messages:
    prefix: str = f"Error when converting {source.austral_name()} to {target.austral_name()}"
    # If the source is a signed integer, and the target is a natural, then we
    # have to ensure the value is >=0.
    if (source.signed and (not target.signed)):
        print( '  if (value < 0) {')
        print(f'      au_abort_internal("{prefix}: value is less than zero, but target type is a natural number.");')
        print( '  }')
    # If the source is strictly larger than the target, check that the value is
    # less than or equal too the maximum value of the target.
    if (source.width > target.width):
        print(f'  if (value > {target.max()}) {{')
        print(f'      au_abort_internal("{prefix}: value is larger than the maximum value of the {target.austral_name()} type.");')
        print( '  }')

def make_int_to_float_conversion(source: IntegerType, target: FloatType):
    # When converting an int to a float we don't need checks... or do we?
    # TODO: ???
    pass

def make_float_to_int_conversion(source: FloatType, target: FloatType):
    # TODO: ???
    pass

def make_float_conversion(source: FloatType, target: FloatType):
    # TODO: ???
    pass

print("\nDeclarations:\n")

make_declarations()

print("\nBodies:\n")

make_bodies()
