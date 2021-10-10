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
end module body.

|code}
