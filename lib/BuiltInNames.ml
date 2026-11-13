(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Common

(* Types *)

type builtin_type =
  | Unit
  | Bool
  | Nat8
  | Nat16
  | Nat32
  | Nat64
  | Int8
  | Int16
  | Int32
  | Int64
  | ByteSize
  | Index
  | Float32
  | Float64
  | Static
  | Fn

let type_name (t: builtin_type): string =
  match t with
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Nat8 -> "Nat8"
  | Nat16 -> "Nat16"
  | Nat32 -> "Nat32"
  | Nat64 -> "Nat64"
  | Int8 -> "Int8"
  | Int16 -> "Int16"
  | Int32 -> "Int32"
  | Int64 -> "Int64"
  | ByteSize -> "ByteSize"
  | Index -> "Index"
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | Static -> "Static"
  | Fn -> "Fn"

(* `type_name` is the table. This list makes it searchable by name. A
   constructor missing from it is a type the parser never recognises. *)
let builtin_types: builtin_type list = [
    Unit; Bool;
    Nat8; Nat16; Nat32; Nat64;
    Int8; Int16; Int32; Int64;
    ByteSize; Index;
    Float32; Float64;
    Static;
    Fn
  ]

let type_of_name (name: string): builtin_type option =
  List.find_opt (fun t -> String.equal (type_name t) name) builtin_types

let read_ref_name: string = "&"
let write_ref_name: string = "&!"
let span_name: string = "Span"
let span_mut_name: string = "Span!"

(* Modules *)

let pervasive_module: string = "Austral.Pervasive"
let memory_module: string = "Austral.Memory"

(* The module the monomorphs of exported functions are generated in, and the
   C unit that holds the wrappers of those functions. *)
let wrappers_module: string = "Austral.Wrappers"
let wrappers_unit_name: string = "Wrappers"

(* Austral.Memory *)

let address_name: string = "Address"
let pointer_name: string = "Pointer"

(* Austral.Pervasive *)

let abort_name: string = "abort"
let root_capability_name: string = "RootCapability"
let exit_code_name: string = "ExitCode"
let exit_success_name: string = "ExitSuccess"
let exit_failure_name: string = "ExitFailure"

let arithmetic_method_name (op: arithmetic_operator): string =
  match op with
  | Add -> "trappingAdd"
  | Subtract -> "trappingSubtract"
  | Multiply -> "trappingMultiply"
  | Divide -> "trappingDivide"

let pervasive_import_names: string list = [
    "Option";
    "Some";
    "None";
    "Either";
    "Left";
    "Right";
    abort_name;
    "spanLength";
    "spanWriteLength";
    root_capability_name;
    "surrenderRoot";
    exit_code_name;
    exit_success_name;
    exit_failure_name;
    "maximum_nat8";
    "maximum_nat16";
    "maximum_nat32";
    "maximum_nat64";
    "minimum_int8";
    "minimum_int16";
    "minimum_int32";
    "minimum_int64";
    "maximum_int8";
    "maximum_int16";
    "maximum_int32";
    "maximum_int64";
    "minimum_bytesize";
    "maximum_bytesize";
    "minimum_index";
    "maximum_index";
    "TrappingArithmetic";
    arithmetic_method_name Add;
    arithmetic_method_name Subtract;
    arithmetic_method_name Multiply;
    arithmetic_method_name Divide;
    "ModularArithmetic";
    "modularAdd";
    "modularSubtract";
    "modularMultiply";
    "modularDivide";
    "BitwiseOperations";
    "bitwiseAnd";
    "bitwiseOr";
    "bitwiseXor";
    "bitwiseNot";
    "Printable";
    "print";
    "printLn";
    "argumentCount";
    "nthArgument";
    "ToNat8";
    "toNat8";
    "ToNat16";
    "toNat16";
    "ToNat32";
    "toNat32";
    "ToNat64";
    "toNat64";
    "ToInt8";
    "toInt8";
    "ToInt16";
    "toInt16";
    "ToInt32";
    "toInt32";
    "ToInt64";
    "toInt64";
    "ToIndex";
    "toIndex";
    "ToFloat32";
    "toFloat32";
    "ToFloat64";
    "toFloat64";
    "Remainder";
    "rem"
  ]

(* Pragmas *)

let foreign_import_pragma_name: string = "Foreign_Import"
let foreign_export_pragma_name: string = "Foreign_Export"
let unsafe_module_pragma_name: string = "Unsafe_Module"
let external_name_argument: string = "External_Name"

(* Names the compiler makes up *)

let fake_parameter_name: string = "fake"
