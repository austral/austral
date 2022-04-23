(** The Austral.Memory module. *)

(** The source code of the Austral.Memory module interface file. *)
let memory_interface_source = {code|

module Austral.Memory is
    type Address[T: Type]: Free;

    generic [T: Type]
    function Null_Pointer(): Address[T];

    generic [T: Type]
    function Allocate(size: Natural_64): Address[T];

    generic [T: Type]
    function Load(address: Address[T]): T;

    generic [T: Type]
    function Store(address: Address[T], value: T): Unit;

    generic [T: Type]
    function Deallocate(address: Address[T]): Unit;

    generic [T: Type, R: Region]
    function Load_Read_Reference(ref: Reference[Address[T], R]): Reference[T, R];

    generic [T: Type, R: Region]
    function Load_Write_Reference(ref: WriteReference[Address[T], R]): WriteReference[T, R];

    generic [T: Type]
    function Resize_Array(array: Address[T], size: Natural_64): Address[T];

    generic [T: Type, U: Type]
    function memmove(source: Address[T], destination: Address[U], count: Natural_64): Unit;

    generic [T: Type, U: Type]
    function memcpy(source: Address[T], destination: Address[U], count: Natural_64): Unit;

    generic [T: Type]
    function Positive_Offset(pointer: Address[T], offset: Natural_64): Address[T];

    generic [T: Type]
    function Negative_Offset(pointer: Address[T], offset: Natural_64): Address[T];
end module.

|code}

(** The source code of the Austral.Memory module body file. *)
let memory_body_source = {code|

module body Austral.Memory is
    type Address[T: Type]: Free is Unit;

    generic [T: Type]
    function Null_Pointer(): Address[T] is
        return @embed(Address[T], "NULL");
    end;

    generic [T: Type]
    function Load(address: Address[T]): T is
        return @embed(T, "*($1)", address);
    end;

    generic [T: Type]
    function Store(address: Address[T], value: T): Unit is
        @embed(Unit, "AU_STORE($1, $2)", address, value);
        return nil;
    end;

    generic [T: Type]
    function Deallocate(address: Address[T]): Unit is
        @embed(Unit, "au_free($1)", address);
        return nil;
    end;

    generic [T: Type, R: Region]
    function Load_Read_Reference(ref: Reference[Address[T], R]): Reference[T, R] is
        return @embed(Reference[T, R], "*($1)", ref);
    end;

    generic [T: Type, R: Region]
    function Load_Write_Reference(ref: WriteReference[Address[T], R]): WriteReference[T, R] is
        return @embed(WriteReference[T, R], "*($1)", ref);
    end;

    generic [T: Type]
    function Allocate(size: Natural_64): Address[T] is
        return @embed(Address[T], "au_calloc($1, $2)", sizeof(T), size);
    end;

    generic [T: Type]
    function Resize_Array(array: Address[T], size: Natural_64): Address[T] is
        let total: Natural_64 := (sizeof(T)) * size;
        return @embed(Address[T], "au_realloc($1, $2)", array, total);
    end;

    generic [T: Type, U: Type]
    function memmove(source: Address[T], destination: Address[U], count: Natural_64): Unit is
        @embed(Address[T], "au_memmove($1, $2, $3)", destination, source, count);
        return nil;
    end;

    generic [T: Type, U: Type]
    function memcpy(source: Address[T], destination: Address[U], count: Natural_64): Unit is
        @embed(Address[T], "au_memcpy($1, $2, $3)", destination, source, count);
        return nil;
    end;

    generic [T: Type]
    function Positive_Offset(pointer: Address[T], offset: Natural_64): Address[T] is
        return @embed(Address[T], "$1 + $2", pointer, offset);
    end;

    generic [T: Type]
    function Negative_Offset(pointer: Address[T], offset: Natural_64): Address[T] is
        return @embed(Address[T], "$1 - $2", pointer, offset);
    end;
end module body.

|code}
