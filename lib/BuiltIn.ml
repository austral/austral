open Identifier
open Type
open Cst
open Semantic

let memory_module_name = make_mod_name "Austral.Memory"

let pointer_type_name = make_ident "Pointer"

let memory_module =
  let i = make_ident in
  let pointer_type_qname = make_qident (memory_module_name, pointer_type_name, pointer_type_name) in
  let typarams = [TypeParameter(i "T", TypeUniverse)]
  and type_t = TyVar (TypeVariable (i "T", TypeUniverse)) in
  let pointer_t = NamedType (pointer_type_qname, [type_t], FreeUniverse) in
  let pointer_type_def =
    (* type Pointer[T: Type]: Free is Unit *)
    STypeAliasDefinition (
        TypeVisOpaque,
        pointer_type_name,
        typarams,
        FreeUniverse,
        Unit
      )
  in
  let allocate_def =
    (* generic T: Type
       function Allocate(value: T): Pointer[T] *)
    SFunctionDeclaration (
        VisPublic,
        i "Allocate",
        typarams,
        [ValueParameter (i "value", type_t)],
        NamedType (pointer_type_qname, [type_t], FreeUniverse)
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
  let decls = [pointer_type_def; allocate_def; load_def; store_def; deallocate_def] in
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

let pervasive_module_name = make_mod_name "Austral.Pervasive"

let pervasive_source_text = (
  {code|
    module Austral.Pervasive is
        union Option[T: Type]: Type is
            case None;
            case Some is
                value: T;
        end;
    end module.
  |code},
  {code|
    module body Austral.Pervasive is
    end module body.
  |code}
)

let option_type_name = make_ident "Option"

let pervasive_imports =
  ConcreteImportList (
      pervasive_module_name,
      [
        ConcreteImport (option_type_name, None)
      ]
    )
