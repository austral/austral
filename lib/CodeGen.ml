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

module Errors = struct
  let foreign_returns_array () =
    austral_raise DeclarationError [
      Text "Foreign functions cannot return arrays."
    ]
end

(* Name generation *)

let counter = ref 0

let new_variable _: string =
  let v = "tmp" ^ (string_of_int !counter) in
  counter := !counter + 1;
  v

(* Identifiers *)

let austral_prefix: string = ""

let gen_ident (i: identifier): string =
  austral_prefix ^ (ident_string i)

let gen_module_name (n: module_name): string =
  austral_prefix ^ replace_char (mod_name_string n) '.' "__"

let gen_qident (i: qident): string =
  (gen_module_name (source_module_name i)) ^ "____" ^ (gen_ident (original_name i))

let gen_sident (mn: module_name) (i: identifier): string =
  (gen_module_name mn) ^ "____" ^ (gen_ident i)

let gen_decl_id (id: decl_id): string =
  let (DeclId i) = id in
  austral_prefix ^ "decl_" ^ (string_of_int i)

let gen_mono_id (id: mono_id): string =
  let (MonoId i) = id in
  austral_prefix ^ "mono_" ^ (string_of_int i)

let gen_ins_meth_id (id: ins_meth_id): string =
  let (InsMethId i) = id in
  austral_prefix ^ "meth_" ^ (string_of_int i)

(* Types *)

let fn_type: c_ty = CNamedType "au_fnptr_t"

let rec gen_type (ty: mono_ty): c_ty =
  match ty with
  | MonoUnit ->
     CNamedType "au_unit_t"
  | MonoBoolean ->
     CNamedType "au_bool_t"
  | MonoInteger (s, w) ->
     let sgn: string =
       match s with
       | Unsigned -> "au_nat"
       | Signed -> "au_int"
     in
     let name: string =
       match w with
       | Width8 -> sgn ^ "8_t"
       | Width16 -> sgn ^ "16_t"
       | Width32 -> sgn ^ "32_t"
       | Width64 -> sgn ^ "64_t"
       | WidthByteSize -> "size_t"
       | WidthIndex -> "au_index_t"
     in
     CNamedType name
  | MonoSingleFloat ->
     CNamedType "float"
  | MonoDoubleFloat ->
     CNamedType "double"
  | MonoNamedType id ->
     CNamedType (gen_mono_id id)
  | MonoStaticArray _ ->
     CNamedType "au_array_t"
  | MonoRegionTy _ ->
     CNamedType "au_region_t"
  | MonoReadRef (t, _) ->
     CPointer (gen_type t)
  | MonoWriteRef (t, _) ->
     CPointer (gen_type t)
  | MonoAddress t ->
     CPointer (gen_type t)
  | MonoPointer t ->
     CPointer (gen_type t)
  | MonoFnPtr _ ->
     fn_type
  | MonoRegionTyVar _ ->
     internal_err "Invalid C type"

(* Expressions *)

let c_string_type = CPointer (CNamedType "uint8_t")

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
let union_tag_enum_name (id: mono_id): string =
  (gen_mono_id id) ^ "_tag"

let local_union_tag_enum_name_from_id (id: mono_id): string =
  (gen_mono_id id) ^ "_tag"

(* Given a union type and the name of a case, return the
   value of the tag enum for that case. *)
let union_tag_value (ty: mono_ty) (case_name: identifier): c_expr =
  CVar ((union_tag_enum_name (union_type_id ty)) ^ "_" ^ (gen_ident case_name))

let rec gen_exp (mn: module_name) (e: mexpr): c_expr =
  let g = gen_exp mn in
  match e with
  | MNilConstant ->
     CBool false
  | MBoolConstant b ->
     CBool b
  | MIntConstant i ->
     CInt i
  | MFloatConstant f ->
     CFloat f
  | MStringConstant s ->
     CFuncall (
         "au_make_array_from_string",
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
     CCast (CStructInitializer (List.map (fun (n, v) -> (gen_ident n, g v)) values), gen_type ty)
  | MUnionConstructor (ty, case_name, values) ->
     let args = CStructInitializer (List.map (fun (n, v) -> (gen_ident n, g v)) values) in
     CCast (CStructInitializer [
                ("tag", union_tag_value ty case_name);
                ("data", CStructInitializer [(gen_ident case_name, args)])
              ], gen_type ty)
  | MEmbed (ty, expr, args) ->
     CEmbed (gen_type ty, expr, List.map g args)
  | MDeref e ->
     CDeref (g e)
  | MTypecast (e, ty) ->
     CCast (g e, gen_type ty)
  | MSizeOf ty ->
     CSizeOf (gen_type ty)
  | MSlotAccessor (expr, slot, _) ->
     let expr = g expr in
     CAddressOf (CStructAccessor (CDeref expr, gen_ident slot))
  | MPointerSlotAccessor (expr, slot, _) ->
     let expr = g expr in
     CAddressOf (CPointerStructAccessor (CDeref expr, gen_ident slot))
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
         CFuncall ("au_array_index", [expr; idx; CSizeOf elem_ty]),
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
     and bs' = List.map (fun (MonoBinding { name; ty; rename; }) -> CLet (gen_ident rename, gen_type ty, Some (CStructAccessor (CVar tmp, gen_ident name)))) bs
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
         CLet (gen_ident v, CNamedType "size_t", Some (ge i));
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
       CStructAccessor (CVar var, "tag")
    | CaseRef ->
       CPointerStructAccessor (CVar var, "tag")
  in
  let switch = CSwitch (accessor, cases) in
  CBlock [
      CLet (var, gen_type ty, Some (gen_exp mn e));
      switch
    ]

and when_to_case (mn: module_name) (ty: mono_ty) (var: string) (case_ref: case_ref) (MTypedWhen (n, bindings, body)): c_switch_case =
  let case_name = gen_ident n
  and tag_value = union_tag_value ty n
  in
  let get_binding binding_name =
    match case_ref with
    | CasePlain ->
       CStructAccessor (CStructAccessor (CStructAccessor (CVar var, "data"), case_name), gen_ident binding_name)
    | CaseRef ->
       CAddressOf (CStructAccessor (CStructAccessor (CStructAccessor (CDeref (CVar var), "data"), case_name), gen_ident binding_name))
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
  List.map (fun (MonoSlot (n, t)) -> CSlot (gen_ident n, gen_type t)) slots

let gen_cases (cases: mono_case list) =
  List.map (fun (MonoCase (n, ss)) -> CSlot (gen_ident n, CStructType (CStruct (None, gen_slots ss)))) cases

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
       (* CConstantDefinition (d, gen_sident mn n, gen_type ty, gen_exp mn e) *)
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
                        local_union_tag_enum_name_from_id id,
                        List.map (fun (MonoCase (n', _)) -> (gen_mono_id id) ^ "_tag_" ^ (gen_ident n')) cases
                      )
     and union_def = CNamedStructDefinition (
                         union_d,
                         mono_id,
                         [
                           CSlot ("tag", CNamedType (local_union_tag_enum_name_from_id id));
                           CSlot ("data", CUnionType (gen_cases cases))
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
           err "Not allowed"
        | MonoBoolean ->
           gen_type t
        | MonoInteger _ ->
           gen_type t
        | MonoSingleFloat ->
           gen_type t
        | MonoDoubleFloat ->
           gen_type t
        | MonoNamedType _ ->
           err "Not allowed"
        | MonoStaticArray (MonoInteger (Unsigned, Width8)) ->
           c_string_type
        | MonoStaticArray _ ->
           err "Not allowed"
        | MonoRegionTy _ ->
           err "Not allowed"
        | MonoReadRef _ ->
           err "Not allowed"
        | MonoWriteRef _ ->
           err "Not allowed"
        | MonoAddress _ ->
           gen_type t
        | MonoPointer _ ->
           gen_type t
        | MonoFnPtr _ ->
           gen_type t
        | MonoRegionTyVar _ ->
           err "Not allowed")
     in
     let return_type_to_c_type t =
       match t with
       | MonoStaticArray _ ->
          Errors.foreign_returns_array ()
       | _ ->
          param_type_to_c_type t
     in
     let ff_params = List.map (fun (MValueParameter (n, t)) -> CValueParam (gen_ident n, param_type_to_c_type t)) params
     and ff_rt = return_type_to_c_type rt in
     let ff_local_decl = CLocalFunctionDeclaration (underlying, ff_params, ff_rt, LinkageExternal) in
     let make_param (n: identifier) (t: mono_ty) =
       if (gen_type t) = (CNamedType "au_array_t") then
         (* Extract the pointer from the Array struct *)
         CStructAccessor (CVar (gen_ident n), "data")
       else
         CVar (gen_ident n)
     in
     let args = List.map (fun (MValueParameter (n, t)) -> make_param n t) params in
     let funcall = CFuncall (underlying, args) in
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
  | CStructDefinition _ ->
     2
  | CNamedStructDefinition _ ->
     3
  | CTypeDefinition _ ->
     4
  | CConstantDefinition _ ->
     5
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
  | CSlot (_, CStructType (CStruct (_,slots))) -> slots_depth slots decls
  | CSlot (_, CUnionType slots) -> slots_depth slots decls
  | _ -> 0

and get_slots name decl =
  match decl with
  | CNamedStructDefinition (_, decl_name, slots) ->
     if name = decl_name then
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
