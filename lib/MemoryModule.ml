(** The Austral.Memory module. *)

(** The source code of the Austral.Memory module interface file. *)
let memory_interface_source = {code|

module Austral.Memory is
    type Address[T: Type]: Free;
    type Pointer[T: Type]: Free;

    generic [T: Type]
    function Null_Pointer(): Address[T];

    generic [T: Type]
    function Null_Check(address: Address[T]): Option[Pointer[T]];

    generic [T: Type]
    function Allocate(size: Natural_64): Address[T];

    generic [T: Type]
    function Load(pointer: Pointer[T]): T;

    generic [T: Type]
    function Store(pointer: Pointer[T], value: T): Unit;

    generic [T: Type]
    function Deallocate(pointer: Pointer[T]): Unit;

    generic [T: Type, R: Region]
    function Load_Read_Reference(ref: Reference[Pointer[T], R]): Reference[T, R];

    generic [T: Type, R: Region]
    function Load_Write_Reference(ref: WriteReference[Pointer[T], R]): WriteReference[T, R];

    generic [T: Type]
    function Resize_Array(array: Pointer[T], size: Natural_64): Pointer[T];

    generic [T: Type, U: Type]
    function memmove(source: Pointer[T], destination: Pointer[U], count: Natural_64): Unit;

    generic [T: Type, U: Type]
    function memcpy(source: Pointer[T], destination: Pointer[U], count: Natural_64): Unit;

    generic [T: Type]
    function Positive_Offset(pointer: Pointer[T], offset: Natural_64): Pointer[T];

    generic [T: Type]
    function Negative_Offset(pointer: Pointer[T], offset: Natural_64): Pointer[T];
end module.

|code}

(** The source code of the Austral.Memory module body file. *)
let memory_body_source = {code|

module body Austral.Memory is
    type Address[T: Type]: Free is Unit;
    type Pointer[T: Type]: Free is Unit;

    generic [T: Type]
    function Null_Pointer(): Address[T] is
        return @embed(Address[T], "NULL");
    end;

    generic [T: Type]
    function Null_Check(address: Address[T]): Option[Pointer[T]] is
        let n: Address[T] := Null_Pointer();
        if address /= n then
            let ptr: Pointer[T] := @embed(Pointer[T], "$1", address);
            return Some(value => ptr);
        else
            let res: Option[Pointer[T]] := None();
            return res;
        end if;
    end;

    generic [T: Type]
    function Allocate(size: Natural_64): Address[T] is
        return @embed(Address[T], "au_calloc($1, $2)", sizeof(T), size);
    end;

    generic [T: Type]
    function Load(pointer: Pointer[T]): T is
        return @embed(T, "*($1)", pointer);
    end;

    generic [T: Type]
    function Store(pointer: Pointer[T], value: T): Unit is
        @embed(Unit, "AU_STORE($1, $2)", pointer, value);
        return nil;
    end;

    generic [T: Type]
    function Deallocate(pointer: Pointer[T]): Unit is
        @embed(Unit, "au_free($1)", pointer);
        return nil;
    end;

    generic [T: Type, R: Region]
    function Load_Read_Reference(ref: Reference[Pointer[T], R]): Reference[T, R] is
        return @embed(Reference[T, R], "*($1)", ref);
    end;

    generic [T: Type, R: Region]
    function Load_Write_Reference(ref: WriteReference[Pointer[T], R]): WriteReference[T, R] is
        return @embed(WriteReference[T, R], "*($1)", ref);
    end;

    generic [T: Type]
    function Resize_Array(array: Pointer[T], size: Natural_64): Pointer[T] is
        let total: Natural_64 := (sizeof(T)) * size;
        return @embed(Pointer[T], "au_realloc($1, $2)", array, total);
    end;

    generic [T: Type, U: Type]
    function memmove(source: Pointer[T], destination: Pointer[U], count: Natural_64): Unit is
        @embed(Pointer[T], "au_memmove($1, $2, $3)", destination, source, count);
        return nil;
    end;

    generic [T: Type, U: Type]
    function memcpy(source: Pointer[T], destination: Pointer[U], count: Natural_64): Unit is
        @embed(Pointer[T], "au_memcpy($1, $2, $3)", destination, source, count);
        return nil;
    end;

    generic [T: Type]
    function Positive_Offset(pointer: Pointer[T], offset: Natural_64): Pointer[T] is
        return @embed(Pointer[T], "$1 + $2", pointer, offset);
    end;

    generic [T: Type]
    function Negative_Offset(pointer: Pointer[T], offset: Natural_64): Pointer[T] is
        return @embed(Pointer[T], "$1 - $2", pointer, offset);
    end;
end module body.

|code}
