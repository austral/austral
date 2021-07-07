open Identifier
open Type
open BuiltIn
open Tast
open Cpp
open Util
open Error

(* Codegen implementation notes:

   A union like this:

       union Optional_Int is
           case Present is
               value: Integer_32;
           case Absent;
       end union;

   Is represented like this:

       enum Optional_Int_Tag {
           PRESENT,
           ABSENT
       }

       typedef struct {
           Optional_Int_Tag tag;
           union {
               struct { int32_t value; } Present;
               struct {} Absent;
           } data;
       } Optional_Int;

   A case statement like:

       case e of
           when Present(value: Integer_32) do
               f();
           when Absent do
               g();
       end case;

   Is represented like this:

       Optional_Int tmp_0 = e;
       switch (tmp_0) {
           case Optional_Int_Tag.PRESENT:
                int32_t value = tmp_0.data.Present.value;
	        f();
                break;
           case Optional_Int_Tag.ABSENT:
                g();
                break;
       }

*)

(* Name generation *)

let new_variable _ =
  ""

(* Identifiers *)

let gen_ident (i: identifier): string =
  (ident_string i)

let gen_module_name (n: module_name): string =
  replace_char (mod_name_string n) '.' "__"

let gen_qident (i: qident): string =
  (gen_module_name (source_module_name i)) ^ "::" ^ (gen_ident (original_name i))

(* Types *)

let rec gen_type (ty: ty): cpp_ty =
  let t s = CNamedType (s, []) in
  match ty with
  | Unit ->
     t "bool"
  | Boolean ->
     t "bool"
  | Integer (s, w) ->
     let s' = (match s with
               | Unsigned -> "u"
               | Signed -> "")
     and w' = string_of_int (width_int w) in
     t (s' ^ "int" ^ w' ^ "_t")
  | SingleFloat ->
     t "float"
  | DoubleFloat ->
     t "double"
  | NamedType (n, a, _) ->
     gen_named_type n a
  | TyVar (TypeVariable (n, _)) ->
     CNamedType (gen_ident n, [])

and gen_named_type (name: qident) (args: ty list): cpp_ty =
  let mod_name = source_module_name name
  and original = original_name name
  in
  if (mod_name = memory_module_name) && (original = pointer_type_name) then
    match args with
    | [t] ->
       CPointer (gen_type t)
    | _ ->
       err "Invalid Pointer type usage"
  else
    CNamedType (gen_qident name, List.map gen_type args)

(* Expressions *)

let union_type_name = function
  | NamedType (n, _, _) ->
     n
  | _ ->
     err "Internal error: Union is not a named type?"

(* Given the name of a union type, returns the name of the
   union's tag enum name. *)
let union_tag_enum_name (n: qident): string =
  (gen_qident n) ^ "_Tag"

(* Like union_tag_enum_name but for union definitions. *)
let local_union_tag_enum_name (n: identifier): string =
  (gen_ident n) ^ "_Tag"

(* Given a union type and the name of a case, return the
   value of the tag enum for that case. *)
let union_tag_value (ty: ty) (case_name: identifier): cpp_expr =
  CVar (union_tag_enum_name (union_type_name ty) ^ "::" ^ (gen_ident case_name))

let rec gen_exp (e: texpr): cpp_expr =
  let g = gen_exp in
  match e with
  | TNilConstant ->
     CBool false
  | TBoolConstant b ->
     CBool b
  | TIntConstant i ->
     CInt i
  | TFloatConstant f ->
     CFloat f
  | TStringConstant _ ->
     err "TODO"
  | TVariable (n, _) ->
     CVar (gen_ident n)
  | TFuncall (name, args, _) ->
     CFuncall (gen_qident name, List.map g args)
  | TMethodCall (name, _, args, _) ->
     CFuncall (gen_qident name, List.map g args)
  | TArithmetic (op, lhs, rhs) ->
     CArithmetic (op, g lhs, g rhs)
  | TComparison (op, lhs, rhs) ->
     CComparison (op, g lhs, g rhs)
  | TConjunction (lhs, rhs) ->
     CConjunction (g lhs, g rhs)
  | TDisjunction (lhs, rhs) ->
     CDisjunction (g lhs, g rhs)
  | TNegation e ->
     CNegation (g e)
  | TIfExpression (c, t, f) ->
     CIfExpression (g c, g t, g f)
  | TRecordConstructor (_, values) ->
     CStructInitializer (List.map (fun (n, v) -> (gen_ident n, g v)) values)
  | TUnionConstructor (ty, case_name, values) ->
     let args = CStructInitializer (List.map (fun (n, v) -> (gen_ident n, g v)) values) in
     CStructInitializer [
         ("tag", union_tag_value ty case_name);
         ("data", CStructInitializer [(gen_ident case_name, args)])
       ]

(* Statements *)

let rec gen_stmt (stmt: tstmt): cpp_stmt =
  match stmt with
  | TSkip ->
     CBlock []
  | TLet (n, t, v, b) ->
     let l = CLet (gen_ident n, gen_type t, gen_exp v) in
     CBlock [l; gen_stmt b]
  | TAssign (n, v) ->
     CAssign (gen_ident n, gen_exp v)
  | TIf (c, tb, fb) ->
     CIf (gen_exp c, gen_stmt tb, gen_stmt fb)
  | TCase (e, whens) ->
     gen_case (get_type e) e whens
  | TWhile (c, b) ->
     CWhile (gen_exp c, gen_stmt b)
  | TFor (v, i, f, b) ->
     CFor (gen_ident v, gen_exp i, gen_exp f, gen_stmt b)
  | TBlock (a, b) ->
     CBlock [gen_stmt a; gen_stmt b]
  | TDiscarding e ->
     CDiscarding (gen_exp e)
  | TReturn e ->
     CReturn (gen_exp e)

and gen_case (ty: ty) (e: texpr) (whens: typed_when list): cpp_stmt =
  (* Code gen for a case statement: generate a variable, and assign the value
     being pattern-matched to that variable. Generate a switch statement over
     the tag enum. Each when statement that has bindings needs to generate some
     variable assignments for those bindings from the generated variable. *)
  let var = new_variable () in
  let cases = List.map (when_to_case ty var) whens in
  let switch = CSwitch (CVar var, cases) in
  CBlock [
      CLet (var, gen_type ty, gen_exp e);
      switch
    ]

and when_to_case ty var (TypedWhen (n, bindings, body)) =
  let case_name = gen_ident n
  and tag_value = union_tag_value ty n
  in
  let get_binding binding_name =
    CStructAccessor (CStructAccessor (CStructAccessor (CVar var, "data"), case_name), gen_ident binding_name)
  in
  let bindings' = List.map (fun (ValueParameter (n, t)) -> CLet (gen_ident n, gen_type t, get_binding n)) bindings in
  let body'' = CBlock (List.append bindings' [gen_stmt body]) in
  CSwitchCase (tag_value, body'')

(* Declarations *)

let gen_params params =
  List.map (fun (ValueParameter (n, t)) -> CValueParam (gen_ident n, gen_type t)) params

let gen_typarams params =
  List.map (fun (TypeParameter (n, _)) -> CTypeParam (gen_ident n)) params

let gen_slots slots =
  List.map (fun (TypedSlot (n, t)) -> CSlot (gen_ident n, gen_type t)) slots

let gen_cases cases =
  List.map (fun (TypedCase (n, ss)) -> CSlot (gen_ident n, CStructType (CStruct (None, gen_slots ss)))) cases

let gen_method typarams (TypedMethodDef (n, params, rt, body)) =
  CFunctionDefinition (gen_ident n, gen_typarams typarams, gen_params params, gen_type rt, gen_stmt body)

let gen_decl (decl: typed_decl): cpp_decl =
  match decl with
  | TConstant (_, n, ty, e, _) ->
     CConstantDefinition (gen_ident n, gen_type ty, gen_exp e)
  | TTypeAlias (_, n, typarams, _, ty, _) ->
     CTypeDefinition (gen_ident n, gen_typarams typarams, gen_type ty)
  | TRecord (_, n, typarams, _, slots, _) ->
     CStructDefinition (
         gen_typarams typarams,
         CStruct (
             Some (gen_ident n),
             gen_slots slots
           )
       )
  | TUnion (_, n, typarams, _, cases, _) ->
     let enum_def = CEnumDefinition (
                        local_union_tag_enum_name n,
                        List.map (fun (TypedCase (n, _)) -> gen_ident n) cases
                      )
     and union_def = CStructDefinition (
                         gen_typarams typarams,
                         CStruct (
                             Some (gen_ident n),
                             [
                               CSlot ("tag", CNamedType (local_union_tag_enum_name n, []));
                               CSlot ("data", CUnionType (gen_cases cases))
                             ]
                           )
                       )
     in
     CDeclBlock [enum_def; union_def]
  | TFunction (_, name, typarams, params, rt, body, _) ->
     CFunctionDefinition (gen_ident name, gen_typarams typarams, gen_params params, gen_type rt, gen_stmt body)
  | TForeignFunction (_, n, params, rt, underlying, _) ->
     let ff_decl = CFunctionDeclaration (underlying, [], gen_params params, gen_type rt, LinkageExternal) in
     let args = List.map (fun (ValueParameter (n, _)) -> CVar (gen_ident n)) params in
     let funcall = CFuncall (underlying, args) in
     let body = CReturn funcall in
     let def = CFunctionDefinition (gen_ident n, [], gen_params params, gen_type rt, body) in
     CDeclBlock [ff_decl; def]
  | TTypeClass _ ->
     (* Type class declarations are not compiled *)
     CDeclBlock []
  | TInstance (_, _, typarams, _, methods, _) ->
     CDeclBlock (List.map (gen_method typarams) methods)

let gen_module (TypedModule (name, decls)) =
  CNamespace (gen_module_name name, List.map gen_decl decls)
