open Id
open Identifier
open Type
open Env
open MonoType
open Mtast
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
     err "TODO: Codegen for region types"
  | MonoReadRef (t, _) ->
     CPointer (gen_type t)
  | MonoWriteRef (t, _) ->
     CPointer (gen_type t)
  | MonoAddress t ->
     CPointer (gen_type t)
  | MonoPointer t ->
     CPointer (gen_type t)
  | MonoRegionTyVar _ ->
     err "internal"

(* Expressions *)

let c_string_type = CPointer (CNamedType "uint8_t")

let union_type_id = function
  | MonoNamedType id ->
     id
  | _ ->
     err "Internal error: Union is not a named type?"

(* Given the ID of a union type, returns the name of the
   union's tag enum. *)
let union_tag_enum_name (id: mono_id): string =
  (gen_mono_id id) ^ "_tag"

(* Like union_tag_enum_name but for union definitions. *)
let local_union_tag_enum_name (n: identifier): string =
  (gen_ident n) ^ "_tag"

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
  | MConcreteFuncall (id, _, args, _) ->
     CFuncall (gen_decl_id id, List.map g args)
  | MGenericFuncall (id, args, _) ->
     CFuncall (gen_mono_id id, List.map g args)
  | MConcreteMethodCall (id, _, args, _) ->
     CFuncall (gen_ins_meth_id id, List.map g args)
  | MGenericMethodCall (_, id, args, _) ->
     CFuncall (gen_mono_id id, List.map g args)
  | MCast (e, t) ->
     CCast (g e, gen_type t)
  | MArithmetic (op, lhs, rhs) ->
     CArithmetic (op, g lhs, g rhs)
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
  | MRecordConstructor (_, values) ->
     CStructInitializer (List.map (fun (n, v) -> (gen_ident n, g v)) values)
  | MUnionConstructor (ty, case_name, values) ->
     let args = CStructInitializer (List.map (fun (n, v) -> (gen_ident n, g v)) values) in
     CStructInitializer [
         ("tag", union_tag_value ty case_name);
         ("data", CStructInitializer [(gen_ident case_name, args)])
       ]
  | MTypeAliasConstructor (ty, expr) ->
     let ty = gen_type ty
     and expr = CStructInitializer [("value", g expr)] in
     CCast (expr, ty)
  | MPath { head; elems; _ } ->
     let p = gen_path mn (g head) (List.rev elems) in
     (match (get_type head) with
      (* References get wrapped in the address-of operator '&' to match C
         semantics. A path of the form `x.y`, where `x` is a reference, if
         compiled straight to C would evaluate to the type of `y`, rather than
         the type reference-to-`y`. So we turn it into `&x.y` so evaluates to
         reference-to-`y`. *)
      | MonoReadRef _ ->
         CAddressOf p
      | MonoWriteRef _ ->
         CAddressOf p
      | _ ->
         p)
  | MEmbed (ty, expr, args) ->
     CEmbed (gen_type ty, expr, List.map g args)
  | MDeref e ->
     CDeref (g e)
  | MTypecast (e, ty) ->
     CCast (g e, gen_type ty)
  | MSizeOf ty ->
     CSizeOf (gen_type ty)
  | MBorrowExpr (_, name, _, _) ->
     CAddressOf (CVar (gen_ident name))

and gen_path (mn: module_name) (expr: c_expr) (elems: mtyped_path_elem list): c_expr =
  match elems with
  | [elem] ->
     gen_path_elem mn expr elem
  | elem::rest ->
     let expr' = gen_path_elem mn expr elem in
     gen_path mn expr' rest
  | [] ->
     err "Empty path"

and gen_path_elem (mn: module_name) (expr: c_expr) (elem: mtyped_path_elem): c_expr =
  match elem with
  | MSlotAccessor (n, _) ->
     CStructAccessor (expr, gen_ident n)
  | MPointerSlotAccessor (n, _) ->
     CPointerStructAccessor (expr, gen_ident n)
  | MArrayIndex (e, _) ->
     CIndex (CCast (CStructAccessor (expr, "data"), CPointer (gen_type (get_type e))), gen_exp mn e)

(* Statements *)

let rec gen_stmt (mn: module_name) (stmt: mstmt): c_stmt =
  let ge = gen_exp mn
  and gs = gen_stmt mn
  in
  match stmt with
  | MSkip ->
     CBlock []
  | MLet (n, t, v, b) ->
     let l = CLet (gen_ident n, gen_type t, ge v) in
     CBlock [l; gs b]
  | MDestructure (bs, e, b) ->
     let tmp = new_variable () in
     let vardecl = CLet (tmp, gen_type (get_type e), ge e)
     and bs' = List.map (fun (n, t) -> CLet (gen_ident n, gen_type t, CStructAccessor (CVar tmp, gen_ident n))) bs
     and b' = gs b
     in
     CBlock (List.concat [[vardecl]; bs'; [b']])
  | MAssign (lvalue, v) ->
     CAssign (gen_lvalue mn lvalue, ge v)
  | MIf (c, tb, fb) ->
     CIf (ge c, gs tb, gs fb)
  | MCase (e, whens) ->
     gen_case mn e whens
  | MWhile (c, b) ->
     CWhile (ge c, gs b)
  | MFor (v, i, f, b) ->
     CExplicitBlock [
         CLet (gen_ident v, CNamedType "size_t", ge i);
         CFor (gen_ident v, ge f, gs b)
       ]
  | MBorrow { original; rename; orig_type; body; _ } ->
     let is_pointer =
       (match orig_type with
        | MonoAddress _ ->
           true
        | _ ->
           false)
     in
     if is_pointer then
       let l = CLet (gen_ident rename, gen_type orig_type, CVar (gen_ident original)) in
       CBlock [l; gs body]
     else
       let l = CLet (gen_ident rename, CPointer (gen_type orig_type), CAddressOf (CVar (gen_ident original))) in
       CBlock [l; gs body]
  | MBlock (a, b) ->
     CBlock [gs a; gs b]
  | MDiscarding e ->
     CDiscarding (ge e)
  | MReturn e ->
     CReturn (ge e)

and gen_lvalue (mn: module_name) (MTypedLValue (name, elems)) =
  gen_path mn (CVar (gen_ident name)) elems

and gen_case (mn: module_name) (e: mexpr) (whens: mtyped_when list): c_stmt =
  (* Code gen for a case statement: generate a variable, and assign the value
     being pattern-matched to that variable. Generate a switch statement over
     the tag enum. Each when statement that has bindings needs to generate some
     variable assignments for those bindings from the generated variable. *)
  let ty = get_type e
  and var = new_variable () in
  let cases = List.map (when_to_case mn ty var) whens in
  let switch = CSwitch (CStructAccessor (CVar var, "tag"), cases) in
  CBlock [
      CLet (var, gen_type ty, gen_exp mn e);
      switch
    ]

and when_to_case (mn: module_name) (ty: mono_ty) (var: string) (MTypedWhen (n, bindings, body)) =
  let case_name = gen_ident n
  and tag_value = union_tag_value ty n
  in
  let get_binding binding_name =
    CStructAccessor (CStructAccessor (CStructAccessor (CVar var, "data"), case_name), gen_ident binding_name)
  in
  let bindings' = List.map (fun (MValueParameter (n, t)) -> CLet (gen_ident n, gen_type t, get_binding n)) bindings in
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
             err "Internal")
      | _ ->
         err "internal")
  | _ ->
     err "internal"

let rec mono_desc (env: env) (id: mono_id): string =
  let mono = get_mono_or_die env id in
  match mono with
  | MonoTypeAliasDefinition { type_id; tyargs; _ } ->
     render_mono env type_id tyargs
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
     err "internal"

and get_decl_name_or_die (env: env) (id: decl_id): string =
  match get_decl_by_id env id with
  | Some decl ->
     (match (decl_name decl) with
      | Some name ->
         (ident_string name)
      | None ->
         err "decl has no name")
  | None ->
     err "internal"

and tyargs_string (args: mono_ty list): string =
  "["
  ^ (String.concat ", " (List.map show_mono_ty args))
  ^ "]"

and render_mono (env: env) (id: decl_id) (tyargs: mono_ty list): string =
  (get_decl_name_or_die env id) ^ (tyargs_string tyargs)

let gen_decl (env: env) (mn: module_name) (decl: mdecl): c_decl list =
  match decl with
  | MConstant (_, n, ty, e) ->
     let d = Desc "Constant" in
     [
       CConstantDefinition (d, gen_sident mn n, gen_type ty, gen_exp mn e)
     ]
  | MTypeAlias (_, n, ty) ->
     let d = Desc "Type Alias" in
     [
       CStructDefinition (
           d,
           CStruct (
               Some (gen_ident n),
               [
                 CSlot ("value", gen_type ty)
               ]
             )
         )
     ]
  | MTypeAliasMonomorph (id, ty) ->
     let d = Desc ("Type alias monomorph: " ^ (mono_desc env id)) in
     [
       CNamedStructDefinition (
           d,
           gen_mono_id id,
           [
             CSlot ("value", gen_type ty)
           ]
         )
     ]
  | MRecord (id, _, slots) ->
     let d = Desc "Record" in
     [
       CNamedStructDefinition (d, gen_decl_id id, gen_slots slots)
     ]
  | MRecordMonomorph (id, slots) ->
     let d = Desc ("Record monomorph: " ^ (mono_desc env id)) in
     [
       CNamedStructDefinition (d, gen_mono_id id, gen_slots slots)
     ]
  | MUnion (_, n, cases) ->
     let enum_d = Desc "Union tag enum" in
     let union_d = Desc "Union" in
     let enum_def = CEnumDefinition (
                        enum_d,
                        (gen_ident n) ^ "_tag",
                        List.map (fun (MonoCase (n', _)) -> (gen_ident n) ^ "_tag_" ^ (gen_ident n')) cases
                      )
     and union_def = CNamedStructDefinition (
                         union_d,
                         gen_ident n,
                         [
                           CSlot ("tag", CNamedType (local_union_tag_enum_name n));
                           CSlot ("data", CUnionType (gen_cases cases))
                         ]
                       )
     in
     [enum_def; union_def]
  | MUnionMonomorph (id, cases) ->
     let enum_d = Desc ("Union monomorph tag enum: " ^ (mono_desc env id)) in
     let union_d = Desc ("Union monomorph: " ^ (mono_desc env id)) in
     let enum_def = CEnumDefinition (
                        enum_d,
                        local_union_tag_enum_name_from_id id,
                        List.map (fun (MonoCase (n', _)) -> (gen_mono_id id) ^ "_tag_" ^ (gen_ident n')) cases
                      )
     and union_def = CNamedStructDefinition (
                         union_d,
                         gen_mono_id id,
                         [
                           CSlot ("tag", CNamedType (local_union_tag_enum_name_from_id id));
                           CSlot ("data", CUnionType (gen_cases cases))
                         ]
                       )
     in
     [enum_def; union_def]
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
     let decl_d = Desc "Foreign function decl" in
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
        | MonoStaticArray (MonoInteger (Unsigned, Width8)) ->
           c_string_type
        | MonoAddress _ ->
           gen_type t
        | MonoNamedType _ ->
           err "Not implemented"
        | _ ->
           err "Not allowed")
     in
     let return_type_to_c_type t =
       match t with
       | MonoStaticArray _ ->
          err "Foreign functions cannot return arrays."
       | _ ->
          param_type_to_c_type t
     in
     let ff_params = List.map (fun (MValueParameter (n, t)) -> CValueParam (gen_ident n, param_type_to_c_type t)) params
     and ff_rt = return_type_to_c_type rt in
     let ff_decl = CFunctionDeclaration (decl_d, underlying, ff_params, ff_rt, LinkageExternal) in
     let make_param (n: identifier) (t: mono_ty) =
       if (gen_type t) = (CNamedType "au_array_t") then
         (* Extract the pointer from the Array struct *)
         CStructAccessor (CVar (gen_ident n), "data")
       else
         CVar (gen_ident n)
     in
     let args = List.map (fun (MValueParameter (n, t)) -> make_param n t) params in
     let funcall = CFuncall (underlying, args) in
     let body = CReturn funcall in
     let def = CFunctionDefinition (def_d, gen_decl_id id, gen_params mn params, gen_type rt, body) in
     [ff_decl; def]
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
  | CFunctionDeclaration _ ->
     6
  | CFunctionDefinition _ ->
     7

let gen_module (env: env) (MonoModule (name, decls)) =
  let type_decls = gen_type_decls decls
  and fun_decls = List.concat (gen_fun_decls name decls)
  and decls = List.concat (List.map (gen_decl env name) decls) in
  let decls = List.concat [type_decls; fun_decls; decls] in
  let sorter a b = compare (decl_order a) (decl_order b) in
  let sorted_decls = List.sort sorter decls in
  CUnit (mod_name_string name, sorted_decls)
