(* The Austral.Pervasive module. *)

let pervasive_interface_source = {code|

module Austral.Pervasive is
    union Option[T: Type]: Type is
        case None;
        case Some is
            value: T;
    end;

    generic [T: Free, R: Region]
    function Deref(ref: Reference[T, R]): T;

    generic [T: Type]
    function Fixed_Array_Size(arr: Fixed_Array[T]): Natural_64;

    function Abort(message: Fixed_Array[Natural_8]): Unit;

    type Root_Capability : Linear;

    constant Maximum_Natural_8: Natural_8;
    constant Maximum_Natural_16: Natural_16;
    constant Maximum_Natural_32: Natural_32;
    constant Maximum_Natural_64: Natural_64;

    constant Minimum_Integer_8: Integer_8;
    constant Maximum_Integer_8: Integer_8;

    constant Minimum_Integer_16: Integer_16;
    constant Maximum_Integer_16: Integer_16;

    constant Minimum_Integer_32: Integer_32;
    constant Maximum_Integer_32: Integer_32;

    constant Minimum_Integer_64: Integer_64;
    constant Maximum_Integer_64: Integer_64;

    interface Trapping_Arithmetic(T: Type) is
        method Trapping_Add(lhs: T, rhs: T): T;
        method Trapping_Subtract(lhs: T, rhs: T): T;
        method Trapping_Multiply(lhs: T, rhs: T): T;
        method Trapping_Divide(lhs: T, rhs: T): T;
    end;

    interface Overflowing_Arithmetic(T: Type) is
        method Overflowing_Add(lhs: T, rhs: T): T;
        method Overflowing_Subtract(lhs: T, rhs: T): T;
        method Overflowing_Multiply(lhs: T, rhs: T): T;
        method Overflowing_Divide(lhs: T, rhs: T): T;
    end;

    interface Saturating_Arithmetic(T: Type) is
        method Saturating_Add(lhs: T, rhs: T): T;
        method Saturating_Subtract(lhs: T, rhs: T): T;
        method Saturating_Multiply(lhs: T, rhs: T): T;
        method Saturating_Divide(lhs: T, rhs: T): T;
    end;
end module.

|code}

let pervasive_body_source = {code|

module body Austral.Pervasive is
    generic [T: Free, R: Region]
    function Deref(ref: Reference[T, R]): T is
        return @embed(T, "*$1", ref);
    end;

    generic [T: Type]
    function Fixed_Array_Size(arr: Fixed_Array[T]): Natural_64 is
        return @embed(Natural_64, "$1.size", arr);
    end;

    function Abort(message: Fixed_Array[Natural_8]): Unit is
        return @embed(Unit, "Austral__Core::Abort($1)", message);
    end;

    type Root_Capability : Linear is Unit;

    constant Maximum_Natural_8: Natural_8 := @embed(Natural_8, "UINT8_MAX");
    constant Maximum_Natural_16: Natural_16 := @embed(Natural_16, "UINT16_MAX");
    constant Maximum_Natural_32: Natural_32 := @embed(Natural_32, "UINT32_MAX");
    constant Maximum_Natural_64: Natural_64 := @embed(Natural_64, "UINT64_MAX");

    constant Minimum_Integer_8: Integer_8 := @embed(Integer_8, "INT8_MIN");
    constant Maximum_Integer_8: Integer_8 := @embed(Integer_8, "INT8_MAX");

    constant Minimum_Integer_16: Integer_16 := @embed(Integer_16, "INT16_MIN");
    constant Maximum_Integer_16: Integer_16 := @embed(Integer_16, "INT16_MAX");

    constant Minimum_Integer_32: Integer_32 := @embed(Integer_32, "INT32_MIN");
    constant Maximum_Integer_32: Integer_32 := @embed(Integer_32, "INT32_MAX");

    constant Minimum_Integer_64: Integer_64 := @embed(Integer_64, "INT64_MIN");
    constant Maximum_Integer_64: Integer_64 := @embed(Integer_64, "INT64_MAX");
end module body.

|code}
