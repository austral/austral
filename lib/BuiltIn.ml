open Identifier
open Cst
open Names

(* Austral.Pervasive *)

let pervasive_module_name = make_mod_name "Austral.Pervasive"

let pervasive_imports =
  ConcreteImportList (
      pervasive_module_name,
      [
        ConcreteImport (make_ident "Option", None);
        ConcreteImport (make_ident "Some", None);
        ConcreteImport (make_ident "None", None);
        ConcreteImport (make_ident "Either", None);
        ConcreteImport (make_ident "Left", None);
        ConcreteImport (make_ident "Right", None);
        ConcreteImport (make_ident "swap", None);
        ConcreteImport (make_ident "fixedArraySize", None);
        ConcreteImport (make_ident "abort", None);
        ConcreteImport (make_ident "RootCapability", None);
        ConcreteImport (make_ident "surrenderRoot", None);
        ConcreteImport (make_ident "ExitCode", None);
        ConcreteImport (make_ident "ExitSuccess", None);
        ConcreteImport (make_ident "ExitFailure", None);
        ConcreteImport (make_ident "maximum_nat8", None);
        ConcreteImport (make_ident "maximum_nat16", None);
        ConcreteImport (make_ident "maximum_nat32", None);
        ConcreteImport (make_ident "maximum_nat64", None);
        ConcreteImport (make_ident "minimum_int8", None);
        ConcreteImport (make_ident "minimum_int16", None);
        ConcreteImport (make_ident "minimum_int32", None);
        ConcreteImport (make_ident "minimum_int64", None);
        ConcreteImport (make_ident "maximum_int8", None);
        ConcreteImport (make_ident "maximum_int16", None);
        ConcreteImport (make_ident "maximum_int32", None);
        ConcreteImport (make_ident "maximum_int64", None);
        ConcreteImport (make_ident "minimum_bytesize", None);
        ConcreteImport (make_ident "maximum_bytesize", None);
        ConcreteImport (make_ident "minimum_index", None);
        ConcreteImport (make_ident "maximum_index", None);
        ConcreteImport (make_ident "TrappingArithmetic", None);
        ConcreteImport (make_ident "trappingAdd", None);
        ConcreteImport (make_ident "trappingSubtract", None);
        ConcreteImport (make_ident "trappingMultiply", None);
        ConcreteImport (make_ident "trappingDivide", None);
        ConcreteImport (make_ident "ModularArithmetic", None);
        ConcreteImport (make_ident "modularAdd", None);
        ConcreteImport (make_ident "modularSubtract", None);
        ConcreteImport (make_ident "modularMultiply", None);
        ConcreteImport (make_ident "modularDivide", None);
        ConcreteImport (make_ident "BitwiseOperations", None);
        ConcreteImport (make_ident "bitwiseAnd", None);
        ConcreteImport (make_ident "bitwiseOr", None);
        ConcreteImport (make_ident "bitwiseXor", None);
        ConcreteImport (make_ident "bitwiseNot", None);
        ConcreteImport (make_ident "Printable", None);
        ConcreteImport (make_ident "print", None);
        ConcreteImport (make_ident "printLn", None);
        ConcreteImport (make_ident "argumentCount", None);
        ConcreteImport (make_ident "nthArgument", None);
        ConcreteImport (make_ident "ToNat8", None);
        ConcreteImport (make_ident "toNat8", None);
        ConcreteImport (make_ident "ToNat16", None);
        ConcreteImport (make_ident "toNat16", None);
        ConcreteImport (make_ident "ToNat32", None);
        ConcreteImport (make_ident "toNat32", None);
        ConcreteImport (make_ident "ToNat64", None);
        ConcreteImport (make_ident "toNat64", None);
        ConcreteImport (make_ident "ToInt8", None);
        ConcreteImport (make_ident "toInt8", None);
        ConcreteImport (make_ident "ToInt16", None);
        ConcreteImport (make_ident "toInt16", None);
        ConcreteImport (make_ident "ToInt32", None);
        ConcreteImport (make_ident "toInt32", None);
        ConcreteImport (make_ident "ToInt64", None);
        ConcreteImport (make_ident "toInt64", None);
        ConcreteImport (make_ident "ToIndex", None);
        ConcreteImport (make_ident "toIndex", None);
      ]
    )

(* Austral.Memory *)

let memory_module_name = make_mod_name "Austral.Memory"

let is_address_type (name: qident): bool =
  let s = source_module_name name
  and o = original_name name
  in
  (equal_module_name s memory_module_name)
  && (equal_identifier o (make_ident address_name))

let is_pointer_type (name: qident): bool =
  let s = source_module_name name
  and o = original_name name
  in
  (equal_module_name s memory_module_name)
  && (equal_identifier o (make_ident pointer_name))
