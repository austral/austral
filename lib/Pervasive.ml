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

    generic [T: Free, R: Region]
    function Deref_Write(ref: WriteReference[T, R]): T;

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

    constant Minimum_Index: Index;
    constant Maximum_Index: Index;

    interface Trapping_Arithmetic(T: Type) is
        method Trapping_Add(lhs: T, rhs: T): T;
        method Trapping_Subtract(lhs: T, rhs: T): T;
        method Trapping_Multiply(lhs: T, rhs: T): T;
        method Trapping_Divide(lhs: T, rhs: T): T;
    end;

    interface Modular_Arithmetic(T: Type) is
        method Modular_Add(lhs: T, rhs: T): T;
        method Modular_Subtract(lhs: T, rhs: T): T;
        method Modular_Multiply(lhs: T, rhs: T): T;
        method Modular_Divide(lhs: T, rhs: T): T;
    end;
end module.

|code}

let pervasive_body_source = {code|

module body Austral.Pervasive is
    generic [T: Free, R: Region]
    function Deref(ref: Reference[T, R]): T is
        return @embed(T, "*$1", ref);
    end;

    generic [T: Free, R: Region]
    function Deref_Write(ref: WriteReference[T, R]): T is
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

    constant Minimum_Natural_8: Integer_8 := 0;
    constant Maximum_Natural_8: Natural_8 := @embed(Natural_8, "UINT8_MAX");

    constant Minimum_Natural_16: Integer_8 := 0;
    constant Maximum_Natural_16: Natural_16 := @embed(Natural_16, "UINT16_MAX");

    constant Minimum_Natural_32: Integer_8 := 0;
    constant Maximum_Natural_32: Natural_32 := @embed(Natural_32, "UINT32_MAX");

    constant Minimum_Natural_64: Integer_8 := 0;
    constant Maximum_Natural_64: Natural_64 := @embed(Natural_64, "UINT64_MAX");

    constant Minimum_Integer_8: Integer_8 := @embed(Integer_8, "INT8_MIN");
    constant Maximum_Integer_8: Integer_8 := @embed(Integer_8, "INT8_MAX");

    constant Minimum_Integer_16: Integer_16 := @embed(Integer_16, "INT16_MIN");
    constant Maximum_Integer_16: Integer_16 := @embed(Integer_16, "INT16_MAX");

    constant Minimum_Integer_32: Integer_32 := @embed(Integer_32, "INT32_MIN");
    constant Maximum_Integer_32: Integer_32 := @embed(Integer_32, "INT32_MAX");

    constant Minimum_Integer_64: Integer_64 := @embed(Integer_64, "INT64_MIN");
    constant Maximum_Integer_64: Integer_64 := @embed(Integer_64, "INT64_MAX");

    constant Minimum_Index: Index := 0;
    constant Maximum_Index: Index := @embed(Index, "SIZE_MAX");

    ---
    --- Trapping Arithmetic
    ---

    implementation Trapping_Arithmetic(Natural_8) is
        method Trapping_Add(lhs: Natural_8, rhs: Natural_8): Natural_8 is
            let result: Natural_8 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_add_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Add (Natural_8)");
            end if;
            return result;
        end;

        method Trapping_Subtract(lhs: Natural_8, rhs: Natural_8): Natural_8 is
            let result: Natural_8 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_sub_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Subtract (Natural_8)");
            end if;
            return result;
        end;

        method Trapping_Multiply(lhs: Natural_8, rhs: Natural_8): Natural_8 is
            let result: Natural_8 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_mul_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Multiply (Natural_8)");
            end if;
            return result;
        end;

        method Trapping_Divide(lhs: Natural_8, rhs: Natural_8): Natural_8 is
            if rhs = 0 then
                Abort("Division by zero in Trapping_Divide (Natural_8)");
            end if;
            return lhs / rhs;
        end;
    end;

    implementation Trapping_Arithmetic(Integer_8) is
        method Trapping_Add(lhs: Integer_8, rhs: Integer_8): Integer_8 is
            let result: Integer_8 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_add_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Add (Integer_8)");
            end if;
            return result;
        end;

        method Trapping_Subtract(lhs: Integer_8, rhs: Integer_8): Integer_8 is
            let result: Integer_8 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_sub_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Subtract (Integer_8)");
            end if;
            return result;
        end;

        method Trapping_Multiply(lhs: Integer_8, rhs: Integer_8): Integer_8 is
            let result: Integer_8 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_mul_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Multiply (Integer_8)");
            end if;
            return result;
        end;

        method Trapping_Divide(lhs: Integer_8, rhs: Integer_8): Integer_8 is
            if rhs = 0 then
                Abort("Division by zero in Trapping_Divide (Integer_8)");
            end if;
            if (lhs = Minimum_Integer_8) and (rhs = -1) then
                Abort("Overflow in Trapping_Divide (Integer_8)");
            end if;
            return 0;
        end;
    end;

    implementation Trapping_Arithmetic(Natural_16) is
        method Trapping_Add(lhs: Natural_16, rhs: Natural_16): Natural_16 is
            let result: Natural_16 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_add_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Add (Natural_16)");
            end if;
            return result;
        end;

        method Trapping_Subtract(lhs: Natural_16, rhs: Natural_16): Natural_16 is
            let result: Natural_16 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_sub_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Subtract (Natural_16)");
            end if;
            return result;
        end;

        method Trapping_Multiply(lhs: Natural_16, rhs: Natural_16): Natural_16 is
            let result: Natural_16 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_mul_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Multiply (Natural_16)");
            end if;
            return result;
        end;

        method Trapping_Divide(lhs: Natural_16, rhs: Natural_16): Natural_16 is
            if rhs = 0 then
                Abort("Division by zero in Trapping_Divide (Natural_16)");
            end if;
            return lhs / rhs;
        end;
    end;

    implementation Trapping_Arithmetic(Integer_16) is
        method Trapping_Add(lhs: Integer_16, rhs: Integer_16): Integer_16 is
            let result: Integer_16 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_add_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Add (Integer_16)");
            end if;
            return result;
        end;

        method Trapping_Subtract(lhs: Integer_16, rhs: Integer_16): Integer_16 is
            let result: Integer_16 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_sub_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Subtract (Integer_16)");
            end if;
            return result;
        end;

        method Trapping_Multiply(lhs: Integer_16, rhs: Integer_16): Integer_16 is
            let result: Integer_16 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_mul_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Multiply (Integer_16)");
            end if;
            return result;
        end;

        method Trapping_Divide(lhs: Integer_16, rhs: Integer_16): Integer_16 is
            if rhs = 0 then
                Abort("Division by zero in Trapping_Divide (Integer_16)");
            end if;
            if (lhs = Minimum_Integer_16) and (rhs = -1) then
                Abort("Overflow in Trapping_Divide (Integer_16)");
            end if;
            return 0;
        end;
    end;

    implementation Trapping_Arithmetic(Natural_32) is
        method Trapping_Add(lhs: Natural_32, rhs: Natural_32): Natural_32 is
            let result: Natural_32 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_add_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Add (Natural_32)");
            end if;
            return result;
        end;

        method Trapping_Subtract(lhs: Natural_32, rhs: Natural_32): Natural_32 is
            let result: Natural_32 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_sub_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Subtract (Natural_32)");
            end if;
            return result;
        end;

        method Trapping_Multiply(lhs: Natural_32, rhs: Natural_32): Natural_32 is
            let result: Natural_32 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_mul_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Multiply (Natural_32)");
            end if;
            return result;
        end;

        method Trapping_Divide(lhs: Natural_32, rhs: Natural_32): Natural_32 is
            if rhs = 0 then
                Abort("Division by zero in Trapping_Divide (Natural_32)");
            end if;
            return lhs / rhs;
        end;
    end;

    implementation Trapping_Arithmetic(Integer_32) is
        method Trapping_Add(lhs: Integer_32, rhs: Integer_32): Integer_32 is
            let result: Integer_32 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_add_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Add (Integer_32)");
            end if;
            return result;
        end;

        method Trapping_Subtract(lhs: Integer_32, rhs: Integer_32): Integer_32 is
            let result: Integer_32 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_sub_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Subtract (Integer_32)");
            end if;
            return result;
        end;

        method Trapping_Multiply(lhs: Integer_32, rhs: Integer_32): Integer_32 is
            let result: Integer_32 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_mul_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Multiply (Integer_32)");
            end if;
            return result;
        end;

        method Trapping_Divide(lhs: Integer_32, rhs: Integer_32): Integer_32 is
            if rhs = 0 then
                Abort("Division by zero in Trapping_Divide (Integer_32)");
            end if;
            if (lhs = Minimum_Integer_32) and (rhs = -1) then
                Abort("Overflow in Trapping_Divide (Integer_32)");
            end if;
            return 0;
        end;
    end;

    implementation Trapping_Arithmetic(Natural_64) is
        method Trapping_Add(lhs: Natural_64, rhs: Natural_64): Natural_64 is
            let result: Natural_64 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_add_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Add (Natural_64)");
            end if;
            return result;
        end;

        method Trapping_Subtract(lhs: Natural_64, rhs: Natural_64): Natural_64 is
            let result: Natural_64 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_sub_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Subtract (Natural_64)");
            end if;
            return result;
        end;

        method Trapping_Multiply(lhs: Natural_64, rhs: Natural_64): Natural_64 is
            let result: Natural_64 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_mul_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Multiply (Natural_64)");
            end if;
            return result;
        end;

        method Trapping_Divide(lhs: Natural_64, rhs: Natural_64): Natural_64 is
            if rhs = 0 then
                Abort("Division by zero in Trapping_Divide (Natural_64)");
            end if;
            return lhs / rhs;
        end;
    end;

    implementation Trapping_Arithmetic(Integer_64) is
        method Trapping_Add(lhs: Integer_64, rhs: Integer_64): Integer_64 is
            let result: Integer_64 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_add_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Add (Integer_64)");
            end if;
            return result;
        end;

        method Trapping_Subtract(lhs: Integer_64, rhs: Integer_64): Integer_64 is
            let result: Integer_64 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_sub_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Subtract (Integer_64)");
            end if;
            return result;
        end;

        method Trapping_Multiply(lhs: Integer_64, rhs: Integer_64): Integer_64 is
            let result: Integer_64 := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_mul_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Multiply (Integer_64)");
            end if;
            return result;
        end;

        method Trapping_Divide(lhs: Integer_64, rhs: Integer_64): Integer_64 is
            if rhs = 0 then
                Abort("Division by zero in Trapping_Divide (Integer_64)");
            end if;
            if (lhs = Minimum_Integer_64) and (rhs = -1) then
                Abort("Overflow in Trapping_Divide (Integer_64)");
            end if;
            return 0;
        end;
    end;

    implementation Trapping_Arithmetic(Index) is
        method Trapping_Add(lhs: Index, rhs: Index): Index is
            let result: Index := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_add_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Add (Index)");
            end if;
            return result;
        end;

        method Trapping_Subtract(lhs: Index, rhs: Index): Index is
            let result: Index := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_sub_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Subtract (Index)");
            end if;
            return result;
        end;

        method Trapping_Multiply(lhs: Index, rhs: Index): Index is
            let result: Index := 0;
            let did_overflow: Boolean := @embed(Boolean, "__builtin_mul_overflow($1, $2, &$3)", lhs, rhs, result);
            if did_overflow then
                Abort("Overflow in Trapping_Multiply (Index)");
            end if;
            return result;
        end;

        method Trapping_Divide(lhs: Index, rhs: Index): Index is
            if rhs = 0 then
                Abort("Division by zero in Trapping_Divide (Index)");
            end if;
            if (lhs = Minimum_Index) and (rhs = -1) then
                Abort("Overflow in Trapping_Divide (Index)");
            end if;
            return 0;
        end;
    end;

    implementation Trapping_Arithmetic(Double_Float) is
        method Trapping_Add(lhs: Double_Float, rhs: Double_Float): Double_Float is
            return @embed(Double_Float, "$1 + $2", lhs, rhs);
        end;

        method Trapping_Subtract(lhs: Double_Float, rhs: Double_Float): Double_Float is
            return @embed(Double_Float, "$1 - $2", lhs, rhs);
        end;

        method Trapping_Multiply(lhs: Double_Float, rhs: Double_Float): Double_Float is
            return @embed(Double_Float, "$1 * $2", lhs, rhs);
        end;

        method Trapping_Divide(lhs: Double_Float, rhs: Double_Float): Double_Float is
            if rhs = 0.0 then
                Abort("Division by zero in Trapping_Divide (Double_Float)");
            end if;
            return @embed(Double_Float, "$1 / $2", lhs, rhs);
        end;
    end;


    ---
    --- Modular Arithmetic
    ---

    implementation Modular_Arithmetic(Natural_8) is
        method Modular_Add(lhs: Natural_8, rhs: Natural_8): Natural_8 is
            return @embed(Natural_8, "$1 + $2", lhs, rhs);
        end;

        method Modular_Subtract(lhs: Natural_8, rhs: Natural_8): Natural_8 is
            return @embed(Natural_8, "$1 - $2", lhs, rhs);
        end;

        method Modular_Multiply(lhs: Natural_8, rhs: Natural_8): Natural_8 is
            return @embed(Natural_8, "$1 * $2", lhs, rhs);
        end;

        method Modular_Divide(lhs: Natural_8, rhs: Natural_8): Natural_8 is
            if rhs = 0 then
                Abort("Division by zero in Modular_Divide (Natural_8)");
            end if;
            return @embed(Natural_8, "$1 / $2", lhs, rhs);
        end;
    end;

    implementation Modular_Arithmetic(Integer_8) is
        method Modular_Add(lhs: Integer_8, rhs: Integer_8): Integer_8 is
            return @embed(Integer_8, "$1 + $2", lhs, rhs);
        end;

        method Modular_Subtract(lhs: Integer_8, rhs: Integer_8): Integer_8 is
            return @embed(Integer_8, "$1 - $2", lhs, rhs);
        end;

        method Modular_Multiply(lhs: Integer_8, rhs: Integer_8): Integer_8 is
            return @embed(Integer_8, "$1 * $2", lhs, rhs);
        end;

        method Modular_Divide(lhs: Integer_8, rhs: Integer_8): Integer_8 is
            if rhs = 0 then
                Abort("Division by zero in Modular_Divide (Integer_8)");
            end if;
            return @embed(Integer_8, "$1 / $2", lhs, rhs);
        end;
    end;

    implementation Modular_Arithmetic(Natural_16) is
        method Modular_Add(lhs: Natural_16, rhs: Natural_16): Natural_16 is
            return @embed(Natural_16, "$1 + $2", lhs, rhs);
        end;

        method Modular_Subtract(lhs: Natural_16, rhs: Natural_16): Natural_16 is
            return @embed(Natural_16, "$1 - $2", lhs, rhs);
        end;

        method Modular_Multiply(lhs: Natural_16, rhs: Natural_16): Natural_16 is
            return @embed(Natural_16, "$1 * $2", lhs, rhs);
        end;

        method Modular_Divide(lhs: Natural_16, rhs: Natural_16): Natural_16 is
            if rhs = 0 then
                Abort("Division by zero in Modular_Divide (Natural_16)");
            end if;
            return @embed(Natural_16, "$1 / $2", lhs, rhs);
        end;
    end;

    implementation Modular_Arithmetic(Integer_16) is
        method Modular_Add(lhs: Integer_16, rhs: Integer_16): Integer_16 is
            return @embed(Integer_16, "$1 + $2", lhs, rhs);
        end;

        method Modular_Subtract(lhs: Integer_16, rhs: Integer_16): Integer_16 is
            return @embed(Integer_16, "$1 - $2", lhs, rhs);
        end;

        method Modular_Multiply(lhs: Integer_16, rhs: Integer_16): Integer_16 is
            return @embed(Integer_16, "$1 * $2", lhs, rhs);
        end;

        method Modular_Divide(lhs: Integer_16, rhs: Integer_16): Integer_16 is
            if rhs = 0 then
                Abort("Division by zero in Modular_Divide (Integer_16)");
            end if;
            return @embed(Integer_16, "$1 / $2", lhs, rhs);
        end;
    end;

    implementation Modular_Arithmetic(Natural_32) is
        method Modular_Add(lhs: Natural_32, rhs: Natural_32): Natural_32 is
            return @embed(Natural_32, "$1 + $2", lhs, rhs);
        end;

        method Modular_Subtract(lhs: Natural_32, rhs: Natural_32): Natural_32 is
            return @embed(Natural_32, "$1 - $2", lhs, rhs);
        end;

        method Modular_Multiply(lhs: Natural_32, rhs: Natural_32): Natural_32 is
            return @embed(Natural_32, "$1 * $2", lhs, rhs);
        end;

        method Modular_Divide(lhs: Natural_32, rhs: Natural_32): Natural_32 is
            if rhs = 0 then
                Abort("Division by zero in Modular_Divide (Natural_32)");
            end if;
            return @embed(Natural_32, "$1 / $2", lhs, rhs);
        end;
    end;

    implementation Modular_Arithmetic(Integer_32) is
        method Modular_Add(lhs: Integer_32, rhs: Integer_32): Integer_32 is
            return @embed(Integer_32, "$1 + $2", lhs, rhs);
        end;

        method Modular_Subtract(lhs: Integer_32, rhs: Integer_32): Integer_32 is
            return @embed(Integer_32, "$1 - $2", lhs, rhs);
        end;

        method Modular_Multiply(lhs: Integer_32, rhs: Integer_32): Integer_32 is
            return @embed(Integer_32, "$1 * $2", lhs, rhs);
        end;

        method Modular_Divide(lhs: Integer_32, rhs: Integer_32): Integer_32 is
            if rhs = 0 then
                Abort("Division by zero in Modular_Divide (Integer_32)");
            end if;
            return @embed(Integer_32, "$1 / $2", lhs, rhs);
        end;
    end;

    implementation Modular_Arithmetic(Natural_64) is
        method Modular_Add(lhs: Natural_64, rhs: Natural_64): Natural_64 is
            return @embed(Natural_64, "$1 + $2", lhs, rhs);
        end;

        method Modular_Subtract(lhs: Natural_64, rhs: Natural_64): Natural_64 is
            return @embed(Natural_64, "$1 - $2", lhs, rhs);
        end;

        method Modular_Multiply(lhs: Natural_64, rhs: Natural_64): Natural_64 is
            return @embed(Natural_64, "$1 * $2", lhs, rhs);
        end;

        method Modular_Divide(lhs: Natural_64, rhs: Natural_64): Natural_64 is
            if rhs = 0 then
                Abort("Division by zero in Modular_Divide (Natural_64)");
            end if;
            return @embed(Natural_64, "$1 / $2", lhs, rhs);
        end;
    end;

    implementation Modular_Arithmetic(Integer_64) is
        method Modular_Add(lhs: Integer_64, rhs: Integer_64): Integer_64 is
            return @embed(Integer_64, "$1 + $2", lhs, rhs);
        end;

        method Modular_Subtract(lhs: Integer_64, rhs: Integer_64): Integer_64 is
            return @embed(Integer_64, "$1 - $2", lhs, rhs);
        end;

        method Modular_Multiply(lhs: Integer_64, rhs: Integer_64): Integer_64 is
            return @embed(Integer_64, "$1 * $2", lhs, rhs);
        end;

        method Modular_Divide(lhs: Integer_64, rhs: Integer_64): Integer_64 is
            if rhs = 0 then
                Abort("Division by zero in Modular_Divide (Integer_64)");
            end if;
            return @embed(Integer_64, "$1 / $2", lhs, rhs);
        end;
    end;

    implementation Modular_Arithmetic(Index) is
        method Modular_Add(lhs: Index, rhs: Index): Index is
            return @embed(Index, "$1 + $2", lhs, rhs);
        end;

        method Modular_Subtract(lhs: Index, rhs: Index): Index is
            return @embed(Index, "$1 - $2", lhs, rhs);
        end;

        method Modular_Multiply(lhs: Index, rhs: Index): Index is
            return @embed(Index, "$1 * $2", lhs, rhs);
        end;

        method Modular_Divide(lhs: Index, rhs: Index): Index is
            if rhs = 0 then
                Abort("Division by zero in Modular_Divide (Index)");
            end if;
            return @embed(Index, "$1 / $2", lhs, rhs);
        end;
    end;
end module body.

|code}
