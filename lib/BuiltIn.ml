open Identifier
open Type
open Cst
open Env

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

let pointer_type_name = make_ident "Pointer"

let is_pointer_type (name: qident): bool =
  let s = source_module_name name
  and o = original_name name
  in
  (equal_module_name s memory_module_name) && (equal_identifier o pointer_type_name)

let add_memory_module (env: env): env =
  (* Add fake files for the module source *)
  let (env, int_file_id) = add_file env { path = ""; contents = "" } in
  let (env, body_file_id) = add_file env { path = ""; contents = "" } in
  (* Add module *)
  let (env, mod_id) = add_module env {
                          name = memory_module_name;
                          interface_file = int_file_id;
                          interface_docstring = Docstring "";
                          body_file = body_file_id;
                          body_docstring = Docstring "";
                          kind = UnsafeModule;
                        }
  in
  (* Utilities *)
  let i = make_ident
  and size_t = Integer (Unsigned, Width64)
  in
  let typarams name = [TypeParameter(i "T", TypeUniverse, name)]
  and type_t name = TyVar (TypeVariable (i "T", TypeUniverse, name))
  in
  let pointer_t name = RawPointer (type_t name)
  in
  (* Add declarations *)
  (* type Pointer[T: Type]: Free is Unit *)
  let (env, _) = add_type_alias env
                   {
                     mod_id = mod_id;
                     vis = TypeVisOpaque;
                     name = pointer_type_name;
                     docstring = Docstring "";
                     typarams = typarams (make_qident (memory_module_name, pointer_type_name, pointer_type_name));
                     universe = FreeUniverse;
                     def = Unit;
                   }
  in
  let (env, _) =
    (* generic T: Type
       function Allocate(value: T): Optional[Pointer[T]] *)
    let name = i "Allocate" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = typarams qname;
        value_params = [ValueParameter (i "value", type_t qname)];
        rt = NamedType (option_type_qname, [pointer_t qname], FreeUniverse);
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic T: Type
       function Load(pointer: Pointer[T]): T *)
    let name = i "Load" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = typarams qname;
        value_params = [ValueParameter (i "pointer", pointer_t qname)];
        rt = type_t qname;
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic T: Type
       function Store(pointer: Pointer[T], value: T): Unit *)
    let name = i "Store" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = typarams qname;
        value_params = [ValueParameter (i "pointer", pointer_t qname); ValueParameter (i "value", type_t qname)];
        rt = Unit;
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic T: Type
       function Deallocate(pointer: Pointer[T]): Unit *)
    let name = i "Deallocate" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = typarams qname;
        value_params = [ValueParameter (i "pointer", pointer_t qname)];
        rt = Unit;
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic [T: Free, R: Region]
       function Load_Read_Reference(ref: Reference[Pointer[T], R]): Reference[T, R] *)
    let name = i "Load_Read_Reference" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = [TypeParameter(i "T", TypeUniverse, qname); TypeParameter (i "R", RegionUniverse, qname)];
        value_params = [ValueParameter (i "ref", ReadRef (RawPointer (type_t qname), TyVar (TypeVariable (i "R", RegionUniverse, qname))))];
        rt = ReadRef (type_t qname, TyVar (TypeVariable (i "R", RegionUniverse, qname)));
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic [T: Free, R: Region]
       function Load_Write_Reference(ref: WriteReference[Pointer[T], R]): WriteReference[T, R] *)
    let name = i "Load_Write_Reference" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = [TypeParameter(i "T", TypeUniverse, qname); TypeParameter (i "R", RegionUniverse, qname)];
        value_params = [ValueParameter (i "ref", WriteRef (RawPointer (type_t qname), TyVar (TypeVariable (i "R", RegionUniverse, qname))))];
        rt = WriteRef (RawPointer (type_t qname), TyVar (TypeVariable (i "R", RegionUniverse, qname)));
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic T: Type
       function Allocate_Array(size: Natural_64): Optional[Pointer[T]] *)
    let name = i "Allocate_Array" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = typarams qname;
        value_params = [ValueParameter (i "size", Integer (Unsigned, Width64))];
        rt = NamedType (option_type_qname, [pointer_t qname], FreeUniverse);
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic T: Type
       function Resize_Array(array: Pointer[T], size: Natural_64): Optional[Pointer[T]] *)
    let name = i "Resize_Array" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = typarams qname;
        value_params = [ValueParameter (i "array", pointer_t qname); ValueParameter (i "size", Integer (Unsigned, Width64))];
        rt = NamedType (option_type_qname, [pointer_t qname], FreeUniverse);
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic [T: Type, U: Type]
       function memmove(source: Pointer[T], destination: Pointer[U], count: Natural_64): Unit *)
    let name = i "memmove" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = [
          TypeParameter(i "T", TypeUniverse, qname);
          TypeParameter(i "U", TypeUniverse, qname)
        ];
        value_params = [
          ValueParameter (i "source", RawPointer (TyVar (TypeVariable (i "T", TypeUniverse, qname))));
          ValueParameter (i "destination", RawPointer (TyVar (TypeVariable (i "U", TypeUniverse, qname))));
          ValueParameter (i "count", size_t)
        ];
        rt = Unit;
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic [T: Type, U: Type]
       function memcpy(source: Pointer[T], destination: Pointer[U], count: Natural_64): Unit *)
    let name = i "memcpy" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = [
          TypeParameter(i "T", TypeUniverse, qname);
          TypeParameter(i "U", TypeUniverse, qname)
        ];
        value_params = [
          ValueParameter (i "source", RawPointer (TyVar (TypeVariable (i "T", TypeUniverse, qname))));
          ValueParameter (i "destination", RawPointer (TyVar (TypeVariable (i "U", TypeUniverse, qname))));
          ValueParameter (i "count", size_t)
        ];
        rt = Unit;
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic T: Type
       functpion Positive_Offset(pointer: Pointer[T], offset: Natural_64): Pointer[T] *)
    let name = i "Positive_Offset" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = typarams qname;
        value_params = [
          ValueParameter (i "array", pointer_t qname);
          ValueParameter (i "offset", Integer (Unsigned, Width64))
        ];
        rt = pointer_t qname;
        external_name = None;
        body = None;
      }
  in
  let (env, _) =
    (* generic T: Type
       functpion Negative_Offset(pointer: Pointer[T], offset: Natural_64): Pointer[T] *)
    let name = i "Negative_Offset" in
    let qname = make_qident (memory_module_name, name, name) in
    add_function env
      {
        mod_id = mod_id;
        vis = VisPublic;
        name = name;
        docstring = Docstring "";
        typarams = typarams qname;
        value_params = [
          ValueParameter (i "array", pointer_t qname);
          ValueParameter (i "offset", Integer (Unsigned, Width64))
        ];
        rt = pointer_t qname;
        external_name = None;
        body = None;
      }
  in
  env
