open Identifier
open Type
open Cst
open Semantic
open Region

(* Austral.Pervasive *)

let pervasive_module_name = make_mod_name "Austral.Pervasive"

let option_type_name = make_ident "Option"

let option_type_qname = make_qident (pervasive_module_name, option_type_name, option_type_name)

let pervasive_module =
  let i = make_ident in
  let option_type_def =
    (*
        union Option[T: Type]: Type is
            case None;
            case Some is
                value: T;
        end;
     *)
    SUnionDefinition (
        pervasive_module_name,
        TypeVisPublic,
        option_type_name,
        [TypeParameter (i "T", TypeUniverse)],
        TypeUniverse,
        [
          TypedCase (i "None", []);
          TypedCase (i "Some", [TypedSlot (i "value", TyVar (TypeVariable (i "T", TypeUniverse)))])
        ]
    )
  and deref_def =
    (* generic T: Free, R: Region
       function Dereference(ref: Reference[T, R]): T *)
    SFunctionDeclaration (
        VisPublic,
        i "Deref",
        [TypeParameter (i "T", FreeUniverse); TypeParameter (i "R", RegionUniverse)],
        [ValueParameter (i "ref", ReadRef (TyVar (TypeVariable (i "T", FreeUniverse)), TyVar (TypeVariable (i "R", RegionUniverse))))],
        TyVar (TypeVariable (i "T", FreeUniverse))
      )
  and fixed_array_size_def =
    (* generic T: Type
       function Fixed_Array_Size(arr: Fixed_Array[T]): Natural_64 *)
    SFunctionDeclaration (
        VisPublic,
        i "Fixed_Array_Size",
        [TypeParameter (i "T", TypeUniverse)],
        [ValueParameter (i "arr", Array (TyVar (TypeVariable (i "T", TypeUniverse)), static_region))],
        Integer (Unsigned, Width64)
      )
  in
  let decls = [option_type_def; deref_def; fixed_array_size_def] in
  SemanticModule {
      name = pervasive_module_name;
      decls = decls;
      imported_classes = [];
      imported_instances = []
    }

let pervasive_imports =
  ConcreteImportList (
      pervasive_module_name,
      [
        ConcreteImport (option_type_name, None);
        ConcreteImport (make_ident "Some", None);
        ConcreteImport (make_ident "None", None);
        ConcreteImport (make_ident "Deref", None);
        ConcreteImport (make_ident "Fixed_Array_Size", None)
      ]
    )

(* Austral.Memory *)

let memory_module_name = make_mod_name "Austral.Memory"

let pointer_type_name = make_ident "Pointer"

let heap_array_type_name = make_ident "Heap_Array"

let memory_module =
  let i = make_ident in
  let pointer_type_qname = make_qident (memory_module_name, pointer_type_name, pointer_type_name)
  and heap_array_type_qname = make_qident (memory_module_name, heap_array_type_name, heap_array_type_name)
  in
  let typarams = [TypeParameter(i "T", TypeUniverse)]
  and type_t = TyVar (TypeVariable (i "T", TypeUniverse))
  in
  let pointer_t = NamedType (pointer_type_qname, [type_t], FreeUniverse)
  and heap_array_t = NamedType (heap_array_type_qname, [type_t], FreeUniverse)
  in
  let pointer_type_def =
    (* type Pointer[T: Type]: Free is Unit *)
    STypeAliasDefinition (
        TypeVisOpaque,
        pointer_type_name,
        typarams,
        FreeUniverse,
        Unit
      )
  and heap_array_type_def =
    (* type Heap_Array[T: Type]: Free is Unit *)
    STypeAliasDefinition (
        TypeVisOpaque,
        heap_array_type_name,
        typarams,
        FreeUniverse,
        Unit
      )
  in
  let allocate_def =
    (* generic T: Type
       function Allocate(value: T): Optional[Pointer[T]] *)
    SFunctionDeclaration (
        VisPublic,
        i "Allocate",
        typarams,
        [ValueParameter (i "value", type_t)],
        NamedType (option_type_qname, [pointer_t], FreeUniverse)
      )
  and load_def =
    (* generic T: Type
       function Load(pointer: Pointer[T]): T *)
    SFunctionDeclaration (
        VisPublic,
        i "Load",
        typarams,
        [ValueParameter (i "pointer", pointer_t)],
        type_t
      )
  and store_def =
    (* generic T: Type
       function Store(pointer: Pointer[T], value: T): Unit *)
    SFunctionDeclaration (
        VisPublic,
        i "Store",
        typarams,
        [ValueParameter (i "pointer", pointer_t); ValueParameter (i "value", type_t)],
        Unit
      )
  and deallocate_def =
    (* generic T: Free
       function Deallocate(pointer: Pointer[T]): Unit *)
    SFunctionDeclaration (
        VisPublic,
        i "Deallocate",
        typarams,
        [ValueParameter (i "pointer", pointer_t)],
        Unit
      )
  in
  let allocate_array_def =
    (* generic T: Type
       function Allocate_Array(size: Natural_64): Optional[Heap_Array[T]] *)
    SFunctionDeclaration (
        VisPublic,
        i "Allocate_Array",
        typarams,
        [ValueParameter (i "size", Integer (Unsigned, Width64))],
        NamedType (option_type_qname, [heap_array_t], FreeUniverse)
      )
  and resize_array_def =
    (* generic T: Type
       functpion Resize_Array(array: Heap_Array[T], size: Natural_64): Optional[Heap_Array[T]] *)
    SFunctionDeclaration (
        VisPublic,
        i "Resize_Array",
        typarams,
        [ValueParameter (i "array", heap_array_t); ValueParameter (i "size", Integer (Unsigned, Width64))],
        NamedType (option_type_qname, [heap_array_t], FreeUniverse)
      )
  and deallocate_array_def =
    (* generic T: Type
       function Deallocate_Array(array: Heap_Array[T]): Unit *)
    SFunctionDeclaration (
        VisPublic,
        i "Deallocate_Array",
        typarams,
        [ValueParameter (i "array", heap_array_t)],
        Unit
      )
  in
  let decls = [
      pointer_type_def;
      heap_array_type_def;
      allocate_def;
      load_def;
      store_def;
      deallocate_def;
      allocate_array_def;
      resize_array_def;
      deallocate_array_def
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

let is_heap_array_type (name: qident): bool =
  let s = source_module_name name
  and o = original_name name
  in
  (equal_module_name s memory_module_name) && (equal_identifier o heap_array_type_name)
