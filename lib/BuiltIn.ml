open Identifier
open Type
open Cst
open Semantic

(* Austral.Pervasive *)

let pervasive_module_name = make_mod_name "Austral.Pervasive"

let option_type_name = make_ident "Option"

let option_type_qname = make_qident (pervasive_module_name, option_type_name, option_type_name)

let root_cap_type_name = make_ident "Root_Capability"

let pervasive_imports =
  ConcreteImportList (
      pervasive_module_name,
      [
        ConcreteImport (option_type_name, None);
        ConcreteImport (make_ident "Some", None);
        ConcreteImport (make_ident "None", None);
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

let pointer_type_name = make_ident "Pointer"

let memory_module =
  let i = make_ident in
  let pointer_type_qname = make_qident (memory_module_name, pointer_type_name, pointer_type_name)
  in
  let typarams name = [TypeParameter(i "T", TypeUniverse, name)]
  and type_t name = TyVar (TypeVariable (i "T", TypeUniverse, name))
  in
  let pointer_t name = NamedType (pointer_type_qname, [type_t name], FreeUniverse)
  in
  let pointer_type_def =
    (* type Pointer[T: Type]: Free is Unit *)
    STypeAliasDefinition (
        TypeVisOpaque,
        pointer_type_name,
        typarams pointer_type_qname,
        FreeUniverse,
        Unit
      )
  in
  let allocate_def =
    let name = i "Allocate" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Type
       function Allocate(value: T): Optional[Pointer[T]] *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "value", type_t qname)],
        NamedType (option_type_qname, [pointer_t qname], FreeUniverse)
      )
  and load_def =
    let name = i "Load" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Type
       function Load(pointer: Pointer[T]): T *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "pointer", pointer_t qname)],
        type_t qname
      )
  and store_def =
    let name = i "Store" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Type
       function Store(pointer: Pointer[T], value: T): Unit *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "pointer", pointer_t qname); ValueParameter (i "value", type_t qname)],
        Unit
      )
  and deallocate_def =
    let name = i "Deallocate" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Free
       function Deallocate(pointer: Pointer[T]): Unit *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "pointer", pointer_t qname)],
        Unit
      )
  and load_read_ref_def =
    let name = i "Load_Read_Reference" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic [T: Free, R: Region]
       function Load_Read_Reference(ref: Reference[Pointer[T], R]): Reference[T, R] *)
    SFunctionDeclaration (
        VisPublic,
        name,
        [TypeParameter(i "T", TypeUniverse, qname); TypeParameter (i "R", RegionUniverse, qname)],
        [ValueParameter (i "ref", ReadRef (NamedType (pointer_type_qname, [type_t qname], FreeUniverse), TyVar (TypeVariable (i "R", RegionUniverse, qname))))],
        ReadRef (NamedType (pointer_type_qname, [type_t qname], FreeUniverse), TyVar (TypeVariable (i "R", RegionUniverse, qname)))
      )
  and load_write_ref_def =
    let name = i "Load_Write_Reference" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic [T: Free, R: Region]
       function Load_Write_Reference(ref: WriteReference[Pointer[T], R]): WriteReference[T, R] *)
    SFunctionDeclaration (
        VisPublic,
        name,
        [TypeParameter(i "T", TypeUniverse, qname); TypeParameter (i "R", RegionUniverse, qname)],
        [ValueParameter (i "ref", WriteRef (NamedType (pointer_type_qname, [type_t qname], FreeUniverse), TyVar (TypeVariable (i "R", RegionUniverse, qname))))],
        WriteRef (NamedType (pointer_type_qname, [type_t qname], FreeUniverse), TyVar (TypeVariable (i "R", RegionUniverse, qname)))
      )
  and allocate_array_def =
    let name = i "Allocate_Array" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Type
       function Allocate_Array(size: Natural_64): Optional[Pointer[T]] *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "size", Integer (Unsigned, Width64))],
        NamedType (option_type_qname, [pointer_t qname], FreeUniverse)
      )
  and resize_array_def =
    let name = i "Resize_Array" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Type
       functpion Resize_Array(array: Pointer[T], size: Natural_64): Optional[Pointer[T]] *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "array", pointer_t qname); ValueParameter (i "size", Integer (Unsigned, Width64))],
        NamedType (option_type_qname, [pointer_t qname], FreeUniverse)
      )
  in
  let decls = [
      pointer_type_def;
      allocate_def;
      load_def;
      store_def;
      deallocate_def;
      load_read_ref_def;
      load_write_ref_def;
      allocate_array_def;
      resize_array_def
    ]
  in
  SemanticModule {
      name = memory_module_name;
      decls = decls;
      imported_classes = [];
      imported_instances = []
    }

let is_pointer_type (name: qident): bool =
  let s = source_module_name name
  and o = original_name name
  in
  (equal_module_name s memory_module_name) && (equal_identifier o pointer_type_name)
