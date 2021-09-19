open Identifier
open Type
open Cst
open Semantic
open Region

(* Austral.Pervasive *)

let pervasive_module_name = make_mod_name "Austral.Pervasive"

let option_type_name = make_ident "Option"

let option_type_qname = make_qident (pervasive_module_name, option_type_name, option_type_name)

let root_cap_type_name = make_ident "Root_Capability"

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
        [TypeParameter (i "T", TypeUniverse, option_type_qname)],
        TypeUniverse,
        [
          TypedCase (i "None", []);
          TypedCase (i "Some", [TypedSlot (i "value", TyVar (TypeVariable (i "T", TypeUniverse, option_type_qname)))])
        ]
    )
  and deref_def =
    let name = i "Deref" in
    let qname = make_qident (pervasive_module_name, name, name) in
    (* generic T: Free, R: Region
       function Dereference(ref: Reference[T, R]): T *)
    SFunctionDeclaration (
        VisPublic,
        name,
        [TypeParameter (i "T", FreeUniverse, qname); TypeParameter (i "R", RegionUniverse, qname)],
        [ValueParameter (i "ref", ReadRef (TyVar (TypeVariable (i "T", FreeUniverse, qname)), TyVar (TypeVariable (i "R", RegionUniverse, qname))))],
        TyVar (TypeVariable (i "T", FreeUniverse, qname))
      )
  and fixed_array_size_def =
    let name = i "Fixed_Array_Size" in
    let qname = make_qident (pervasive_module_name, name, name) in
    (* generic T: Type
       function Fixed_Array_Size(arr: Fixed_Array[T]): Natural_64 *)
    SFunctionDeclaration (
        VisPublic,
        name,
        [TypeParameter (i "T", TypeUniverse, qname)],
        [ValueParameter (i "arr", Array (TyVar (TypeVariable (i "T", TypeUniverse, qname)), static_region))],
        Integer (Unsigned, Width64)
      )
  and abort_def =
    let name = i "Abort" in
    (* function Abort(arr: Fixed_Array[Natural_8]): Unit *)
    SFunctionDeclaration (
        VisPublic,
        name,
        [],
        [ValueParameter (i "arr", Array (Integer (Unsigned, Width8), static_region))],
        Unit
      )
  and root_cap_type_def =
    let name = i "Root_Capability" in
    (* type Root_Capability : Linear; *)
    STypeAliasDefinition (
        TypeVisOpaque,
        name,
        [],
        LinearUniverse,
        Unit
    )
  in
  let decls = [option_type_def; deref_def; fixed_array_size_def; abort_def; root_cap_type_def] in
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
        ConcreteImport (make_ident "Fixed_Array_Size", None);
        ConcreteImport (make_ident "Abort", None);
        ConcreteImport (make_ident "Root_Capability", None)
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
  let typarams name = [TypeParameter(i "T", TypeUniverse, name)]
  and type_t name = TyVar (TypeVariable (i "T", TypeUniverse, name))
  in
  let pointer_t name = NamedType (pointer_type_qname, [type_t name], FreeUniverse)
  and heap_array_t name = NamedType (heap_array_type_qname, [type_t name], FreeUniverse)
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
  and heap_array_type_def =
    (* type Heap_Array[T: Type]: Free is Unit *)
    STypeAliasDefinition (
        TypeVisOpaque,
        heap_array_type_name,
        typarams (make_qident (memory_module_name, heap_array_type_name, heap_array_type_name)),
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
  in
  let allocate_array_def =
    let name = i "Allocate_Array" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Type
       function Allocate_Array(size: Natural_64): Optional[Heap_Array[T]] *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "size", Integer (Unsigned, Width64))],
        NamedType (option_type_qname, [heap_array_t qname], FreeUniverse)
      )
  and resize_array_def =
    let name = i "Resize_Array" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Type
       functpion Resize_Array(array: Heap_Array[T], size: Natural_64): Optional[Heap_Array[T]] *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "array", heap_array_t qname); ValueParameter (i "size", Integer (Unsigned, Width64))],
        NamedType (option_type_qname, [heap_array_t qname], FreeUniverse)
      )
  and deallocate_array_def =
    let name = i "Deallocate_Array" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Type
       function Deallocate_Array(array: Heap_Array[T]): Unit *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "array", heap_array_t qname)],
        Unit
      )
  and heap_array_size_def =
    let name = i "Heap_Array_Size" in
    let qname = make_qident (memory_module_name, name, name) in
    (* generic T: Type
       function Heap_Array_Size(array: Heap_Array[T]): Natural_64 *)
    SFunctionDeclaration (
        VisPublic,
        name,
        typarams qname,
        [ValueParameter (i "array", heap_array_t qname)],
        Integer (Unsigned, Width64)
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
      deallocate_array_def;
      heap_array_size_def
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
