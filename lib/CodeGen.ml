(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Id
open Identifier
open Type
open EnvTypes
open Env
open EnvUtils
open MonoType
open MonoTypeBindings
open Stages.Tast
open Stages.Mtast
open MtastUtil
open CRepr
open Util
open Escape
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
       switch (tmp_0.tag) {
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

let counter = ref 0

let new_variable _: CIdent.t =
  let v = "tmp" ^ (string_of_int !counter) in
  counter := !counter + 1;
  CIdent.generated v

(* Identifiers *)

let austral_prefix: string = ""

let ident_text (i: identifier): string =
  austral_prefix ^ (ident_string i)

(* A variable or a parameter. *)
let gen_ident (i: identifier): CIdent.t =
  CIdent.austral (ident_text i)

(* A record slot or a union case. *)
let gen_member (i: identifier): CIdent.t =
  CIdent.austral_unescaped (ident_text i)

let gen_module_name (n: module_name): string =
  austral_prefix ^ replace_char (mod_name_string n) '.' "__"

(* A constant where it is used. *)
let gen_qident (i: qident): CIdent.t =
  CIdent.austral ((gen_module_name (source_module_name i)) ^ "____" ^ (ident_text (original_name i)))

(* A constant where it is defined. The two spellings differ when the name of
   the module starts with a C reserved word (issue #92). *)
let gen_sident (mn: module_name) (i: identifier): CIdent.t =
  CIdent.austral_unescaped ((gen_module_name mn) ^ "____" ^ (ident_text i))

let gen_decl_id (id: decl_id): CIdent.t =
  let (DeclId i) = id in
  CIdent.generated (austral_prefix ^ "decl_" ^ (string_of_int i))

let mono_id_text (id: mono_id): string =
  let (MonoId i) = id in
  austral_prefix ^ "mono_" ^ (string_of_int i)

let gen_mono_id (id: mono_id): CIdent.t =
  CIdent.generated (mono_id_text id)

let gen_ins_meth_id (id: ins_meth_id): CIdent.t =
  let (InsMethId i) = id in
  CIdent.generated (austral_prefix ^ "meth_" ^ (string_of_int i))

(* The members of the struct that represents a union: see the notes above. *)
let tag_slot: CIdent.t = CIdent.generated "tag"
let data_slot: CIdent.t = CIdent.generated "data"

(* Types *)

let fn_type: c_ty = CBuiltinType CFnPtrType

let gen_integer_type (s: signedness) (w: integer_width): c_ty =
  let sized (unsigned: c_builtin_ty) (signed: c_builtin_ty): c_builtin_ty =
    match s with
    | Unsigned -> unsigned
    | Signed -> signed
  in
  let ty: c_builtin_ty =
    match w with
    | Width8 -> sized CNat8Type CInt8Type
    | Width16 -> sized CNat16Type CInt16Type
    | Width32 -> sized CNat32Type CInt32Type
    | Width64 -> sized CNat64Type CInt64Type
    | WidthByteSize -> CSizeType
    | WidthIndex -> CIndexType
  in
  CBuiltinType ty

let rec gen_type (ty: mono_ty): c_ty =
  match ty with
  | MonoUnit ->
     CBuiltinType CUnitType
  | MonoBoolean ->
     CBuiltinType CBoolType
  | MonoInteger (s, w) ->
     gen_integer_type s w
  | MonoSingleFloat ->
     CBuiltinType CFloatType
  | MonoDoubleFloat ->
     CBuiltinType CDoubleType
  | MonoNamedType id ->
     CNamedType (gen_mono_id id)
  | MonoRegionTy _ ->
     CBuiltinType CRegionType
  | MonoReadRef (t, _) ->
     CPointer (gen_type t)
  | MonoWriteRef (t, _) ->
     CPointer (gen_type t)
  | MonoSpan _ ->
     CBuiltinType CSpanType
  | MonoSpanMut _ ->
     CBuiltinType CSpanType
  | MonoAddress t ->
     CPointer (gen_type t)
  | MonoPointer t ->
     CPointer (gen_type t)
  | MonoFnPtr _ ->
     fn_type
  | MonoRegionTyVar _ ->
     internal_err "Invalid C type"

(* Expressions *)

let union_type_id = function
  | MonoNamedType id ->
     id
  | MonoReadRef (MonoNamedType id, _) ->
     id
  | MonoWriteRef (MonoNamedType id, _) ->
     id
  | _ ->
     internal_err "Union is not a named type?"

(* Given the ID of a union type, returns the name of the
   union's tag enum. *)
let union_tag_enum_text (id: mono_id): string =
  (mono_id_text id) ^ "_tag"

let union_tag_enum_name (id: mono_id): CIdent.t =
  CIdent.generated (union_tag_enum_text id)

(* Given the ID of a union type and the name of a case, returns the name of
   the tag enum's value for that case. *)
let union_tag_value_name (id: mono_id) (case_name: identifier): CIdent.t =
  CIdent.generated ((union_tag_enum_text id) ^ "_" ^ (ident_text case_name))

(* Given a union type and the name of a case, return the
   value of the tag enum for that case. *)
let union_tag_value (ty: mono_ty) (case_name: identifier): c_expr =
  CVar (union_tag_value_name (union_type_id ty) case_name)

let rec gen_exp (mn: module_name) (e: mexpr): c_expr =
  let g = gen_exp mn in
  match e with
  | MNilConstant ->
     CBool false
  | MBoolConstant b ->
     CBool b
  | MIntConstant i ->
     CInt (NumberLiteral.int_text i)
  | MFloatConstant f ->
     CFloat (NumberLiteral.float_text f)
  | MStringConstant s ->
     CFuncall (
         CIdent.generated "au_make_span_from_string",
         [
           CString s;
           CInt (string_of_int (String.length (escaped_to_string s)))
         ]
       )
  | MConstVar (n, _) ->
     CVar (gen_qident n)
  | MParamVar (n, _) ->
     CVar (gen_ident n)
  | MLocalVar (n, _) ->
     CVar (gen_ident n)
  | MTemporary (n, _) ->
     CVar (gen_ident n)
  | MGenericFunVar (id, _) ->
     CCast (CVar (gen_mono_id id), fn_type)
  | MConcreteFunVar (id, _) ->
     CCast (CVar (gen_decl_id id), fn_type)
  | MConcreteFuncall (id, _, args, _) ->
     CFuncall (gen_decl_id id, List.map g args)
  | MGenericFuncall (id, args, _) ->
     CFuncall (gen_mono_id id, List.map g args)
  | MConcreteMethodCall (id, _, args, _) ->
     CFuncall (gen_ins_meth_id id, List.map g args)
  | MGenericMethodCall (_, id, args, _) ->
     CFuncall (gen_mono_id id, List.map g args)
  | MFptrCall (name, args, ty) ->
     let argtys: mono_ty list = List.map get_type args in
     CFptrCall (CVar (gen_ident name), gen_type ty, List.map gen_type argtys, List.map g args)
  | MCast (e, t) ->
     CCast (g e, gen_type t)
  | MComparison (op, lhs, rhs) ->
     CComparison (op, g lhs, g rhs)
  | MConjunction (lhs, rhs) ->
     CConjunction (g lhs, g rhs)
  | MDisjunction (lhs, rhs) ->
     CDisjunction (g lhs, g rhs)
  | MNegation e ->
     CNegation (g e)
  | MIfExpression (c, t, f) ->
     CIfExpression (g c, g t, g f)
  | MRecordConstructor (ty, values) ->
     CCast (CStructInitializer (List.map (fun (n, v) -> (gen_member n, g v)) values), gen_type ty)
  | MUnionConstructor (ty, case_name, values) ->
     let args = CStructInitializer (List.map (fun (n, v) -> (gen_member n, g v)) values) in
     CCast (CStructInitializer [
                (tag_slot, union_tag_value ty case_name);
                (data_slot, CStructInitializer [(gen_member case_name, args)])
              ], gen_type ty)
  | MEmbed (ty, expr, args) ->
     CEmbed (gen_type ty, CSource expr, List.map g args)
  | MDeref e ->
     CDeref (g e)
  | MSizeOf ty ->
     CSizeOf (gen_type ty)
  | MSlotAccessor (expr, slot, _) ->
     let expr = g expr in
     CAddressOf (CStructAccessor (CDeref expr, gen_member slot))
  | MPointerSlotAccessor (expr, slot, _) ->
     let expr = g expr in
     CAddressOf (CPointerStructAccessor (CDeref expr, gen_member slot))
  | MArrayIndex (expr, idx, ty) ->
     let expr = g expr
     and idx = g idx in
     let elem_ty = begin
         match ty with
         | MonoReadRef (ty, _) ->
            ty
         | MonoWriteRef (ty, _) ->
            ty
         | _ ->
            internal_err "Internal error"
       end
     in
     let elem_ty = gen_type elem_ty in
     CCast (
         CFuncall (CIdent.generated "au_array_index", [expr; idx; CSizeOf elem_ty]),
         CPointer elem_ty
       )
  | MSpanIndex (expr, idx, ty) ->
     let expr = g expr
     and idx = g idx in
     let elem_ty = begin
         match ty with
         | MonoReadRef (ty, _) ->
            ty
         | MonoWriteRef (ty, _) ->
            ty
         | _ ->
            internal_err "Internal error"
       end
     in
     let elem_ty = gen_type elem_ty in
     CCast (
         CFuncall (CIdent.generated "au_array_index", [CAddressOf expr; idx; CSizeOf elem_ty]),
         CPointer elem_ty
       )

(* Statements *)

let rec gen_stmt (mn: module_name) (stmt: mstmt): c_stmt =
  let ge = gen_exp mn
  and gs = gen_stmt mn
  in
  match stmt with
  | MSkip ->
     CBlock []
  | MLet (n, t, b) ->
     let l = CLet (gen_ident n, gen_type t, None) in
     CBlock [l; gs b]
  | MDestructure (bs, e, b) ->
     let tmp = new_variable () in
     let vardecl = CLet (tmp, gen_type (get_type e), Some (ge e))
     and bs' = List.map (fun (MonoBinding { name; ty; rename; }) -> CLet (gen_ident rename, gen_type ty, Some (CStructAccessor (CVar tmp, gen_member name)))) bs
     and b' = gs b
     in
     CBlock (List.concat [[vardecl]; bs'; [b']])
  | MAssign (lvalue, rvalue) ->
     CAssign (CDeref (ge lvalue), ge rvalue)
  | MAssignVar (name, rvalue) ->
     CAssign (CVar (gen_ident (local_name name)), ge rvalue)
  | MInitialAssign (name, rvalue) ->
     CAssign (CVar (gen_ident (local_name name)), ge rvalue)
  | MLetTmp (name, ty, expr) ->
    let l = CLet (gen_ident name, gen_type ty, Some (ge expr)) in
    l
  | MAssignTmp (name, value) ->
    CAssign (CVar (gen_ident name), ge value)
  | MIf (c, tb, fb) ->
     CIf (ge c, gs tb, gs fb)
  | MCase (e, whens, case_ref) ->
     gen_case mn e whens case_ref
  | MWhile (c, b) ->
     CWhile (ge c, gs b)
  | MFor (v, i, f, b) ->
     CExplicitBlock [
         CLet (gen_ident v, CBuiltinType CSizeType, Some (ge i));
         CFor (gen_ident v, ge f, gs b)
       ]
  | MBorrow { original; rename; orig_type; body; mode; _ } ->
     let is_pointer =
       (match orig_type with
        | MonoAddress _ ->
           true
        | _ ->
           false)
     in
     if is_pointer then
       let l = CLet (gen_ident rename, gen_type orig_type, Some (CVar (gen_ident original))) in
       CBlock [l; gs body]
     else
       let e = begin
           match mode with
           | Read ->
              CAddressOf (CVar (gen_ident original))
           | Write ->
              CAddressOf (CVar (gen_ident original))
           | Reborrow ->
              CVar (gen_ident original)
         end
       in
       let ty = begin
           match mode with
           | Read ->
              CPointer (gen_type orig_type)
           | Write ->
              CPointer (gen_type orig_type)
           | Reborrow ->
              gen_type orig_type
         end
       in
       let l = CLet (gen_ident rename, ty, Some e) in
       CBlock [l; gs body]
  | MBlock (a, b) ->
     CBlock [gs a; gs b]
  | MDiscarding e ->
     CDiscarding (ge e)
  | MReturn e ->
     CReturn (ge e)

and gen_case (mn: module_name) (e: mexpr) (whens: mtyped_when list) (case_ref: case_ref): c_stmt =
  (* Code gen for a case statement: generate a variable, and assign the value
     being pattern-matched to that variable. Generate a switch statement over
     the tag enum. Each when statement that has bindings needs to generate some
     variable assignments for those bindings from the generated variable. *)
  let ty = get_type e
  and var = new_variable () in
  let cases = List.map (when_to_case mn ty var case_ref) whens in
  let accessor =
    match case_ref with
    | CasePlain ->
       CStructAccessor (CVar var, tag_slot)
    | CaseRef ->
       CPointerStructAccessor (CVar var, tag_slot)
  in
  let switch = CSwitch (accessor, cases) in
  CBlock [
      CLet (var, gen_type ty, Some (gen_exp mn e));
      switch
    ]

and when_to_case (mn: module_name) (ty: mono_ty) (var: CIdent.t) (case_ref: case_ref) (MTypedWhen (n, bindings, body)): c_switch_case =
  let case_name = gen_member n
  and tag_value = union_tag_value ty n
  in
  let get_binding binding_name =
    match case_ref with
    | CasePlain ->
       CStructAccessor (CStructAccessor (CStructAccessor (CVar var, data_slot), case_name), gen_member binding_name)
    | CaseRef ->
       CAddressOf (CStructAccessor (CStructAccessor (CStructAccessor (CDeref (CVar var), data_slot), case_name), gen_member binding_name))
  in
  let f (MonoBinding { name; ty; rename; }) =
    let ty =
      match case_ref with
      | CasePlain ->
         gen_type ty
      | CaseRef ->
         CPointer (gen_type ty)
    in
    CLet (gen_ident rename, ty, Some (get_binding name))
  in
  let bindings' = List.map f bindings in
  let body'' = CExplicitBlock (List.append bindings' [gen_stmt mn body]) in
  CSwitchCase (tag_value, body'')

(* Declarations *)

let gen_params mn params =
  let _ = mn in
  List.map (fun (MValueParameter (n, t)) -> CValueParam (gen_ident n, gen_type t)) params

let gen_slots (slots: mono_slot list) =
  List.map (fun (MonoSlot (n, t)) -> CSlot (gen_member n, gen_type t)) slots

let gen_cases (cases: mono_case list) =
  List.map (fun (MonoCase (n, ss)) -> CSlot (gen_member n, CStructType (gen_slots ss))) cases

let gen_method (mn: module_name) (MConcreteMethod (id, _, params, rt, body)) =
  let d = Desc "Method" in
  CFunctionDefinition (d, gen_ins_meth_id id, gen_params mn params, gen_type rt, gen_stmt mn body)

let get_original_module_name (env: env) (id: mono_id): module_name =
  match get_monomorph env id with
  | Some (MonoFunction { function_id; _ }) ->
     (match get_decl_by_id env function_id with
      | Some (Function { mod_id; _ }) ->
         (match get_module_by_id env mod_id with
          | Some (ModRec { name; _ }) ->
             name
          | _ ->
             internal_err "Couldn't get module record")
      | _ ->
         internal_err "Couldn't get function")
  | _ ->
     internal_err "Couldn't get monomorph"

let rec mono_desc (env: env) (id: mono_id): string =
  let mono = get_mono_or_die env id in
  match mono with
  | MonoRecordDefinition { type_id; tyargs; _ } ->
     render_mono env type_id tyargs
  | MonoUnionDefinition { type_id; tyargs; _ } ->
     render_mono env type_id tyargs
  | MonoFunction { function_id; tyargs; _ } ->
     render_mono env function_id tyargs
  | MonoInstanceMethod _ ->
     ""

and get_mono_or_die (env: env) (id: mono_id): monomorph =
  match get_monomorph env id with
  | Some mono ->
     mono
  | None ->
     internal_err "Couldn't get monomorph"

and get_decl_name_or_die (env: env) (id: decl_id): string =
  match get_decl_by_id env id with
  | Some decl ->
     (match (decl_name decl) with
      | Some name ->
         (ident_string name)
      | None ->
         internal_err "decl has no name")
  | None ->
     internal_err "Couldn't find decl"

and tyargs_string (args: mono_type_bindings): string =
  "["
  ^ (String.concat ", " (List.map (fun (_, ty) -> show_mono_ty ty) (mono_bindings_as_list args)))
  ^ "]"

and render_mono (env: env) (id: decl_id) (tyargs: mono_type_bindings): string =
  (get_decl_name_or_die env id) ^ (tyargs_string tyargs)

let gen_decl (env: env) (mn: module_name) (decl: mdecl): c_decl list =
  match decl with
  | MConstant (_, n, _, e) ->
     let d = Desc "Constant" in
     [
       CMacro (d, gen_sident mn n, gen_exp mn e)
     ]
  | MRecord _ ->
     []
  | MRecordMonomorph (id, slots) ->
     let d = Desc ("Record monomorph: " ^ (mono_desc env id))
     and id = gen_mono_id id in
     [
       CStructForwardDeclaration (d, id);
       CNamedStructDefinition (d, id, gen_slots slots)
     ]
  | MUnion _ ->
     []
  | MUnionMonomorph (id, cases) ->
     let mono_id = gen_mono_id id in
     let enum_d = Desc ("Union monomorph tag enum: " ^ (mono_desc env id)) in
     let union_d = Desc ("Union monomorph: " ^ (mono_desc env id)) in
     let enum_def = CEnumDefinition (
                        enum_d,
                        union_tag_enum_name id,
                        List.map (fun (MonoCase (n', _)) -> union_tag_value_name id n') cases
                      )
     and union_def = CNamedStructDefinition (
                         union_d,
                         mono_id,
                         [
                           CSlot (tag_slot, CNamedType (union_tag_enum_name id));
                           CSlot (data_slot, CUnionType (gen_cases cases))
                         ]
                       )
     in
     [
       enum_def;
       CStructForwardDeclaration (union_d, mono_id);
       union_def
     ]
  | MFunction (id, _, params, rt, body) ->
     let d = Desc "Function" in
     [
       CFunctionDefinition (d, gen_decl_id id, gen_params mn params, gen_type rt, gen_stmt mn body)
     ]
  | MFunctionMonomorph (id, params, rt, body) ->
     (* Load-bearing hack: prefix parameters not with the current module name,
        but with the name of the module the monomorph's declaration is from. *)
     let mn': module_name = get_original_module_name env id in
     let d = Desc ("Function monomorph: " ^ (mono_desc env id)) in
     [
       CFunctionDefinition (d, gen_mono_id id, gen_params mn' params, gen_type rt, gen_stmt mn body)
     ]
  | MForeignFunction (id, _, params, rt, underlying) ->
     let def_d = Desc "Foreign function" in
     let param_type_to_c_type (t: mono_ty): c_ty =
       (match t with
        | MonoUnit ->
           generic_error ForeignFunctionTypeNotAllowed
        | MonoBoolean ->
           gen_type t
        | MonoInteger _ ->
           gen_type t
        | MonoSingleFloat ->
           gen_type t
        | MonoDoubleFloat ->
           gen_type t
        | MonoNamedType _ ->
           generic_error ForeignFunctionTypeNotAllowed
        | MonoRegionTy _ ->
           generic_error ForeignFunctionTypeNotAllowed
        | MonoReadRef _ ->
           generic_error ForeignFunctionTypeNotAllowed
        | MonoWriteRef _ ->
           generic_error ForeignFunctionTypeNotAllowed
        | MonoSpan (ty, _) ->
           CPointer (gen_type ty)
        | MonoSpanMut (ty, _) ->
           CPointer (gen_type ty)
        | MonoAddress _ ->
           gen_type t
        | MonoPointer _ ->
           gen_type t
        | MonoFnPtr _ ->
           gen_type t
        | MonoRegionTyVar _ ->
           generic_error ForeignFunctionTypeNotAllowed)
     in
     let return_type_to_c_type t =
       match t with
       | _ ->
          param_type_to_c_type t
     in
     (* The parameters of the `extern` declaration are not escaped, and the
        name is escaped where it is called and not where it is declared
        (issue #92). *)
     let ff_params = List.map (fun (MValueParameter (n, t)) -> CValueParam (CIdent.austral_unescaped (ident_text n), param_type_to_c_type t)) params
     and ff_rt = return_type_to_c_type rt in
     let ff_local_decl = CLocalFunctionDeclaration (CIdent.external_name underlying, ff_params, ff_rt, LinkageExternal) in
     let make_param (n: identifier) (t: mono_ty) =
       match gen_type t with
       | CBuiltinType CSpanType ->
          (* Extract the pointer from the Array struct *)
          CStructAccessor (CVar (gen_ident n), CIdent.generated "data")
       | CBuiltinType (CUnitType | CBoolType
                       | CNat8Type | CNat16Type | CNat32Type | CNat64Type
                       | CInt8Type | CInt16Type | CInt32Type | CInt64Type
                       | CSizeType | CIndexType | CFloatType | CDoubleType
                       | CRegionType | CFnPtrType)
       | CNamedType _ | CPointer _ | CStructType _ | CUnionType _ ->
          CVar (gen_ident n)
     in
     let args = List.map (fun (MValueParameter (n, t)) -> make_param n t) params in
     let funcall = CFuncall (CIdent.external_escaped underlying, args) in
     let body = CBlock [
                    ff_local_decl;
                    CReturn funcall
                  ] in
     let def = CFunctionDefinition (def_d, gen_decl_id id, gen_params mn params, gen_type rt, body) in
     [def]
  | MConcreteInstance (_, _, _, methods) ->
     List.map (gen_method mn) methods
  | MMethodMonomorph (id, params, rt, body) ->
     let d = Desc ("Method monomorph: " ^ (mono_desc env id)) in
     [
       CFunctionDefinition (d, gen_mono_id id, gen_params mn params, gen_type rt, gen_stmt mn body)
     ]

(* Extract types into forward type declarations *)

let rec gen_type_decls decls =
  List.filter_map gen_type_decl decls

and gen_type_decl decl =
  (*let d n = Some (CStructForwardDeclaration (gen_decl_id n)) in*)
  match decl with
  (*| MRecord (id, _, _) ->
     d id*)
  (*| MUnion (id, _, _) ->
     d id*)
  | _ ->
     None

(* Extract functions into forward function declarations *)

let rec gen_fun_decls mn decls =
  List.filter_map (gen_fun_decl mn) decls

and gen_fun_decl mn decl =
  match decl with
  | MFunction (id, _, p, rt, _) ->
     let d = Desc "Function forward declaratioon" in
     Some [CFunctionDeclaration (d, gen_decl_id id, gen_params mn p, gen_type rt, LinkageInternal)]
  | MFunctionMonomorph (id, p, rt, _) ->
     let d = Desc "Function monomorph forward declaration" in
     Some [CFunctionDeclaration (d, gen_mono_id id, gen_params mn p, gen_type rt, LinkageInternal)]
  | MForeignFunction (id, _, p, rt, _) ->
     let d = Desc "Foreign function forward declaration" in
     Some [CFunctionDeclaration (d, gen_decl_id id, gen_params mn p, gen_type rt, LinkageInternal)]
  | MConcreteInstance (_, _, _, ms) ->
     Some (List.map (gen_method_decl mn) ms)
  | MMethodMonomorph (id, p, rt, _) ->
     let d = Desc "Method monomorph forward declaration" in
     Some [CFunctionDeclaration (d, gen_mono_id id, gen_params mn p, gen_type rt, LinkageInternal)]
  | _ ->
     None

and gen_method_decl mn (MConcreteMethod (id, _, params, rt, _)) =
  let d = Desc "Method forward declaration" in
  CFunctionDeclaration (d, gen_ins_meth_id id, gen_params mn params, gen_type rt, LinkageInternal)

(* Codegen a module *)

let decl_order = function
  | CEnumDefinition _ ->
     0
  | CStructForwardDeclaration _ ->
     1
  | CNamedStructDefinition _ ->
     3
  | CMacro _ ->
     5
  | CFunctionDeclaration _ ->
     6
  | CFunctionDefinition _ ->
     7

(* the following code is used to sort declarations in dependency order, so that
   the C compiler doesn't complain about types being incomplete *)

let rec slot_depth decls (slot: c_slot) =
  match slot with
  | CSlot (_, CNamedType name) -> name_depth name decls
  (* The depth of a named type that has no definition among the declarations. *)
  | CSlot (_, CBuiltinType _) -> 1
  | CSlot (_, CStructType slots) -> slots_depth slots decls
  | CSlot (_, CUnionType slots) -> slots_depth slots decls
  | CSlot (_, CPointer _) -> 0

and get_slots name decl =
  match decl with
  | CNamedStructDefinition (_, decl_name, slots) ->
     if CIdent.equal name decl_name then
       Some slots
     else
       None
  | _ -> None

and name_depth name decls =
  let slots =
    match (List.filter_map (get_slots name) decls) with
    | hd :: _ -> hd
    | _ -> [] in
  slots_depth slots decls

and slots_depth slots decls =
  let deps = List.map (slot_depth decls) slots in
  let dep_depth = List.fold_left max 0 deps in
  let depth = 1 + dep_depth in
  depth

let detail_compare decls a b =
  match (a, b) with
  | (CNamedStructDefinition (_, _, s1), CNamedStructDefinition (_, _, s2)) ->
     (slots_depth s1 decls) - (slots_depth s2 decls)
  | _ ->
     compare (decl_order a) (decl_order b)

(* end declaration sorting code *)

let gen_module (env: env) (MonoModule (name, decls)) =
  let type_decls = gen_type_decls decls
  and fun_decls = List.concat (gen_fun_decls name decls)
  and decls = List.concat (List.map (gen_decl env name) decls) in
  let decls = List.concat [type_decls; fun_decls; decls] in
  let sorted_decls = List.sort (detail_compare decls) decls in
  CUnit (mod_name_string name, sorted_decls)
