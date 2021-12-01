open Identifier
open Type
open BuiltIn
open Tast
open Region
open Cpp
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

let new_variable _ =
  let v = "tmp" ^ (string_of_int !counter) in
  counter := !counter + 1;
  v

(* Identifiers *)

let austral_prefix: string = "A_"

let gen_ident (i: identifier): string =
  austral_prefix ^ (ident_string i)

let gen_module_name (n: module_name): string =
  austral_prefix ^ replace_char (mod_name_string n) '.' "__"

let gen_qident (i: qident): string =
  (gen_module_name (source_module_name i)) ^ "::" ^ (gen_ident (original_name i))

(* Utilities for compiling special types and expressions *)

let is_optional_pointer_named_type (name: qident) (args: ty list): ty option =
  if ((source_module_name name) = pervasive_module_name) && ((original_name name) = option_type_name) then
    (match args with
     | [NamedType (n', [arg'], _)] ->
        if is_pointer_type n' then
          Some arg'
        else
          None
     | _ ->
        None)
  else
    None

(* If ty is of the form Optional[Pointer[T]], return Some T, otherwise None. *)
let is_optional_pointer_type (ty: ty): ty option =
  match ty with
  | NamedType (name, args, _) ->
     is_optional_pointer_named_type name args
  | _ ->
     None

(* Types *)

let rec gen_type (ty: ty): cpp_ty =
  let t s = CNamedType (s, []) in
  match ty with
  | Unit ->
     t "bool"
  | Boolean ->
     t "bool"
  | Integer (s, w) ->
     (match w with
      | WidthIndex ->
         t "size_t"
      | _ ->
         let s' = (match s with
                   | Unsigned -> "u"
                   | Signed -> "")
         and w' = string_of_int (width_int w) in
         t (s' ^ "int" ^ w' ^ "_t"))
  | SingleFloat ->
     t "float"
  | DoubleFloat ->
     t "double"
  | NamedType (n, a, _) ->
     gen_named_type n a
  | Array (t, _) ->
     CNamedType ("Austral__Core::Array", [gen_type t])
  | RegionTy _ ->
     err "TODO: Codegen for region types"
  | ReadRef (t, _) ->
     CPointer (gen_type t)
  | WriteRef (t, _) ->
     CPointer (gen_type t)
  | TyVar (TypeVariable (n, _, _)) ->
     CNamedType (gen_ident n, [])

and gen_named_type (name: qident) (args: ty list): cpp_ty =
  (* Pointer types are compiled specially *)
  if is_pointer_type name then
    match args with
    | [t] ->
       CPointer (gen_type t)
    | _ ->
       err "Invalid Pointer type usage"
  else
    if is_heap_array_type name then
      match args with
      | [t] ->
         CNamedType ("Austral__Core::Array", [gen_type t])
      | _ ->
         err "Invalid Heap_Array type usage."
    else
      (* Option[Pointer[T]] types are compiled specially *)
      match is_optional_pointer_named_type name args with
      | Some target_ty ->
         CPointer (gen_type target_ty)
      | None ->
         (* It's a regular user defined type. *)
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

let c_string_type = CPointer (CNamedType ("uint8_t", []))

let rec gen_exp (mn: module_name) (e: texpr): cpp_expr =
  let g = gen_exp mn in
  match e with
  | TNilConstant ->
     CBool false
  | TBoolConstant b ->
     CBool b
  | TIntConstant i ->
     CInt i
  | TFloatConstant f ->
     CFloat f
  | TStringConstant s ->
     CFuncall ("Austral__Core::Make_Array", [CInt (string_of_int (String.length (escaped_to_string s))); CCast (CString s, c_string_type)], [])
  | TVariable (n, _) ->
     if (equal_module_name (source_module_name n) mn) then
       CVar (gen_ident (original_name n))
     else
       CVar (gen_qident n)
  | TFuncall (name, args, _, substs) ->
     CFuncall (gen_qident name, List.map g args, List.map (fun (_, t) -> gen_type t) substs)
  | TMethodCall (name, _, args, _) ->
     CFuncall (gen_qident name, List.map g args, [])
  | TCast (e, t) ->
     CCast (g e, gen_type t)
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
     (match is_optional_pointer_type ty with
      | Some _ ->
         (* Constructors for values of type Option[Pointer[T]] must be compiled specially *)
         if (equal_identifier case_name (make_ident "Some")) then
           match values with
           | [(_, v)] ->
              g v
           | _ ->
              err "Bad constructor"
         else
           if (equal_identifier case_name (make_ident "None")) then
             CVar "NULL"
           else
             err "Invalid case"
      | None ->
         let args = CStructInitializer (List.map (fun (n, v) -> (gen_ident n, g v)) values) in
         CStructInitializer [
             ("tag", union_tag_value ty case_name);
             ("data", CStructInitializer [(gen_ident case_name, args)])
           ])
  | TPath { head; elems; _ } ->
     let p = gen_path mn (g head) (List.rev elems) in
     (match (get_type head) with
      (* References get wrapped in the address-of operator '&' to match C
         semantics. A path of the form `x.y`, where `x` is a reference, if
         compiled straight to C would evaluate to the type of `y`, rather than
         the type reference-to-`y`. So we turn it into `&x.y` so evaluates to
         reference-to-`y`. *)
      | ReadRef _ ->
         CAddressOf p
      | WriteRef _ ->
         CAddressOf p
      | _ ->
         p)
  | TEmbed (ty, expr, args) ->
     CEmbed (gen_type ty, expr, List.map g args)
  | TDeref e ->
     CDeref (g e)
  | TTypecast (e, ty) ->
     CCast (g e, gen_type ty)

and gen_path (mn: module_name) (expr: cpp_expr) (elems: typed_path_elem list): cpp_expr =
  match elems with
  | [elem] ->
     gen_path_elem mn expr elem
  | elem::rest ->
     let expr' = gen_path_elem mn expr elem in
     gen_path mn expr' rest
  | [] ->
     err "Empty path"

and gen_path_elem (mn: module_name) (expr: cpp_expr) (elem: typed_path_elem): cpp_expr =
  match elem with
  | TSlotAccessor (n, _) ->
     CStructAccessor (expr, gen_ident n)
  | TPointerSlotAccessor (n, _) ->
     CPointerStructAccessor (expr, gen_ident n)
  | TArrayIndex (e, _) ->
     CIndex (expr, gen_exp mn e)

(* Statements *)

let rec gen_stmt (mn: module_name) (stmt: tstmt): cpp_stmt =
  let ge = gen_exp mn
  and gs = gen_stmt mn
  in
  match stmt with
  | TSkip _ ->
     CBlock []
  | TLet (_, n, t, v, b) ->
     let l = CLet (gen_ident n, gen_type t, ge v) in
     CBlock [l; gs b]
  | TDestructure (_, bs, e, b) ->
     let tmp = new_variable () in
     let vardecl = CLet (tmp, gen_type (get_type e), ge e)
     and bs' = List.map (fun (n, t) -> CLet (gen_ident n, gen_type t, CStructAccessor (CVar tmp, gen_ident n))) bs
     and b' = gs b
     in
     CBlock (List.concat [[vardecl]; bs'; [b']])
  | TAssign (_, lvalue, v) ->
     CAssign (gen_lvalue mn lvalue, ge v)
  | TIf (_, c, tb, fb) ->
     CIf (ge c, gs tb, gs fb)
  | TCase (_, e, whens) ->
     gen_case mn e whens
  | TWhile (_, c, b) ->
     CWhile (ge c, gs b)
  | TFor (_, v, i, f, b) ->
     CFor (gen_ident v, ge i, ge f, gs b)
  | TBorrow { original; rename; orig_type; body; _ } ->
     let is_pointer =
       (match orig_type with
        | NamedType (name, _, _) ->
           is_pointer_type name
        | _ ->
           false)
     in
     if is_pointer then
       let l = CLet (gen_ident rename, gen_type orig_type, CVar (gen_ident original)) in
       CBlock [l; gs body]
     else
       let l = CLet (gen_ident rename, CPointer (gen_type orig_type), CAddressOf (CVar (gen_ident original))) in
       CBlock [l; gs body]
  | TBlock (_, a, b) ->
     CBlock [gs a; gs b]
  | TDiscarding (_, e) ->
     CDiscarding (ge e)
  | TReturn (_, e) ->
     CReturn (ge e)

and gen_lvalue mn (TypedLValue (name, elems)) =
  gen_path mn (CVar (gen_ident name)) elems

and gen_case (mn: module_name) (e: texpr) (whens: typed_when list): cpp_stmt =
  (* If the expression is of type Option[Pointer[T]], we compile this specially. *)
  let ty = get_type e in
  match is_optional_pointer_type ty with
  | Some _ ->
     gen_option_pointer_case mn e whens
  | None ->
     gen_ordinary_case mn e whens

and gen_option_pointer_case (mn: module_name) (e: texpr) (whens: typed_when list): cpp_stmt =
  (* Codegen: a case statement of the form:

         case optptr of
             when Some(value: Pointer[T]) do
                 f(value);
             when None do
                 g();
         end case;

     Compiles to:

         T* tmp = optptr;
         if (tmp != NULL) {
             T* value = tmp;
             f(value);
         } else {
             g();
         }
   *)
  let is_some (TypedWhen (name, _, body)): tstmt option =
    if (equal_identifier name (make_ident "Some")) then
      Some body
    else
      None
  and is_none (TypedWhen (name, _, body)): tstmt option =
    if (equal_identifier name (make_ident "None")) then
      Some body
    else
      None
  in
  let ty = get_type e
  and var = new_variable () in
  match List.find_map is_some whens with
  | Some some_body ->
     (match List.find_map is_none whens with
      | Some none_body ->
         let cond = CComparison (NotEqual, CVar var, CVar "NULL")
         and tb = CBlock [
                      CLet (austral_prefix ^ "value", gen_type ty, CVar var);
                      gen_stmt mn some_body
                    ]
         in
         let ifstmt = CIf (cond, tb, gen_stmt mn none_body) in
         CBlock [
             CLet (var, gen_type ty, gen_exp mn e);
             ifstmt
           ]
      | _ ->
         err "No None case")
  | None ->
     err "No Some case"


and gen_ordinary_case (mn: module_name) (e: texpr) (whens: typed_when list): cpp_stmt =
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

and when_to_case mn ty var (TypedWhen (n, bindings, body)) =
  let case_name = gen_ident n
  and tag_value = union_tag_value ty n
  in
  let get_binding binding_name =
    CStructAccessor (CStructAccessor (CStructAccessor (CVar var, "data"), case_name), gen_ident binding_name)
  in
  let bindings' = List.map (fun (ValueParameter (n, t)) -> CLet (gen_ident n, gen_type t, get_binding n)) bindings in
  let body'' = CExplicitBlock (List.append bindings' [gen_stmt mn body]) in
  CSwitchCase (tag_value, body'')

(* Declarations *)

let gen_params params =
  List.map (fun (ValueParameter (n, t)) -> CValueParam (gen_ident n, gen_type t)) params

let gen_typarams params =
  let f (TypeParameter (n, u, _)) =
    if u = RegionUniverse then
      None
    else
      Some (CTypeParam (gen_ident n))
  in
  List.filter_map f params

let gen_slots slots =
  List.map (fun (TypedSlot (n, t)) -> CSlot (gen_ident n, gen_type t)) slots

let gen_cases cases =
  List.map (fun (TypedCase (n, ss)) -> CSlot (gen_ident n, CStructType (CStruct (None, gen_slots ss)))) cases

let gen_method mn typarams (TypedMethodDef (n, params, rt, body)) =
  CFunctionDefinition (gen_ident n, gen_typarams typarams, gen_params params, gen_type rt, gen_stmt mn body)

let gen_decl (mn: module_name) (decl: typed_decl): cpp_decl list =
  match decl with
  | TConstant (_, n, ty, e, _) ->
     [CConstantDefinition (gen_ident n, gen_type ty, gen_exp mn e)]
  | TTypeAlias (_, n, typarams, _, ty, _) ->
     [CTypeDefinition (gen_ident n, gen_typarams typarams, gen_type ty)]
  | TRecord (_, n, typarams, _, slots, _) ->
     [
       CStructDefinition (
           gen_typarams typarams,
           CStruct (
               Some (gen_ident n),
               gen_slots slots
             )
         )
     ]
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
     [enum_def; union_def]
  | TFunction (_, name, typarams, params, rt, body, _) ->
     [CFunctionDefinition (gen_ident name, gen_typarams typarams, gen_params params, gen_type rt, gen_stmt mn body)]
  | TForeignFunction (_, n, params, rt, underlying, _) ->
     let param_type_to_c_type t =
       (match t with
        | Unit ->
           err "Not allowed"
        | Boolean ->
           gen_type t
        | Integer _ ->
           gen_type t
        | SingleFloat ->
           gen_type t
        | DoubleFloat ->
           gen_type t
        | Array (Integer (Unsigned, Width8), r) ->
           if r = static_region then
             c_string_type
           else
             err "Not allowed"
        | NamedType (n, args, _) ->
           if is_pointer_type n then
             gen_type t
           else
             (match is_optional_pointer_named_type n args with
              | Some _ ->
                 gen_type t
              | None ->
                 err "Pointers and optional pointers are the only named type allowed as parameters of C functions.")
        | _ ->
           err "Not allowed")
     in
     let return_type_to_c_type t =
       match t with
       | Array _ ->
          err "Foreign functions cannot return arrays."
       | _ ->
          param_type_to_c_type t
     in
     let ff_params = List.map (fun (ValueParameter (n, t)) -> CValueParam (gen_ident n, param_type_to_c_type t)) params
     and ff_rt = return_type_to_c_type rt in
     let ff_decl = CFunctionDeclaration (underlying, [], ff_params, ff_rt, LinkageExternal) in
     let make_param n t =
       if t = string_type then
         (* Extract the pointer from the Array struct *)
         CStructAccessor (CVar (gen_ident n), "data")
       else
         CVar (gen_ident n)
     in
     let args = List.map (fun (ValueParameter (n, t)) -> make_param n t) params in
     let funcall = CFuncall (underlying, args, []) in
     let body = CReturn funcall in
     let def = CFunctionDefinition (gen_ident n, [], gen_params params, gen_type rt, body) in
     [ff_decl; def]
  | TTypeClass _ ->
     (* Type class declarations are not compiled *)
     []
  | TInstance (_, _, typarams, _, methods, _) ->
     List.map (gen_method mn typarams) methods

(* Extract types into forward type declarations *)

let rec gen_type_decls decls =
  List.filter_map gen_type_decl decls

and gen_type_decl decl =
  let d n p = Some (CStructForwardDeclaration (gen_ident n, gen_typarams p)) in
  match decl with
  | TRecord (_, n, p, _ ,_, _) ->
     d n p
  | TUnion (_, n, p, _, _ ,_) ->
     d n p
  | _ ->
     None

(* Extract functions into forward function declarations *)

let rec gen_fun_decls decls =
  List.filter_map gen_fun_decl decls

and gen_fun_decl decl =
  match decl with
  | TFunction (_, n, tp, p, rt, _, _) ->
     Some [CFunctionDeclaration (gen_ident n, gen_typarams tp, gen_params p, gen_type rt, LinkageInternal)]
  | TForeignFunction (_, n, p, rt, _, _) ->
     Some [CFunctionDeclaration (gen_ident n, [], gen_params p, gen_type rt, LinkageInternal)]
  | TInstance (_, _, tp, _, ms, _) ->
     Some (List.map (gen_method_decl tp) ms)
  | _ ->
     None

and gen_method_decl typarams (TypedMethodDef (n, params, rt, _)) =
  CFunctionDeclaration (gen_ident n, gen_typarams typarams, gen_params params, gen_type rt, LinkageInternal)

(* Codegen a module *)

let decl_order = function
  | CNamespace _ ->
     0
  | CUsingDeclaration _ ->
     1
  | CEnumDefinition _ ->
     2
  | CStructForwardDeclaration _ ->
     3
  | CStructDefinition _ ->
     4
  | CTypeDefinition _ ->
     5
  | CConstantDefinition _ ->
     6
  | CFunctionDeclaration _ ->
     7
  | CFunctionDefinition _ ->
     8

let gen_module (TypedModule (name, decls)) =
  let type_decls = gen_type_decls decls
  and fun_decls = List.concat (gen_fun_decls decls)
  and decls' = List.concat (List.map (gen_decl name) decls) in
  let decls'' = List.concat [type_decls; fun_decls; decls'] in
  let sorter a b = compare (decl_order a) (decl_order b) in
  let sorted_decls = List.sort sorter decls'' in
  CNamespace (gen_module_name name, sorted_decls)
