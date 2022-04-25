open Identifier
open Cst

(* Austral.Pervasive *)

let pervasive_module_name = make_mod_name "Austral.Pervasive"

let root_cap_type_name = make_ident "Root_Capability"

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
        ConcreteImport (make_ident "Deref", None);
        ConcreteImport (make_ident "Deref_Write", None);
        ConcreteImport (make_ident "Fixed_Array_Size", None);
        ConcreteImport (make_ident "Abort", None);
        ConcreteImport (make_ident "Root_Capability", None);
        ConcreteImport (make_ident "Trapping_Arithmetic", None);
        ConcreteImport (make_ident "Trapping_Add", None);
        ConcreteImport (make_ident "Trapping_Subtract", None);
        ConcreteImport (make_ident "Trapping_Multiply", None);
        ConcreteImport (make_ident "Trapping_Divide", None);
        ConcreteImport (make_ident "Modular_Arithmetic", None);
        ConcreteImport (make_ident "Modular_Add", None);
        ConcreteImport (make_ident "Modular_Subtract", None);
        ConcreteImport (make_ident "Modular_Multiply", None);
        ConcreteImport (make_ident "Modular_Divide", None)
      ]
    )

(* Austral.Memory *)

let memory_module_name = make_mod_name "Austral.Memory"
