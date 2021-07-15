open OUnit2
open Austral_core.Identifier
open Austral_core.Type
open Austral_core.TypeBindings
open Austral_core.TypeMatch
open TestUtil

let meq (ty: ty) (ty': ty) (bindlist: (string * ty) list) =
  let bindings = match_type ty ty' in
  let list = bindings_list bindings in
  let list' = List.sort (fun (n, _) (n', _) -> compare (ident_string n) (ident_string n')) list in
  let list'' = List.map (fun (n, t) -> (ident_string n, t)) list' in
  eq list'' bindlist

let v n u =
  TyVar (TypeVariable (make_ident n, u))

let i = make_ident

let test_successful_matches _ =
  let t = v "T" FreeUniverse
  and fnt n a = NamedType (make_qident (make_mod_name "", i n, i n), a, FreeUniverse) in
  meq Unit Unit [];
  meq t Unit [("T", Unit)];
  let param = fnt "Option" [t]
  and arg = fnt "Option" [Unit] in
  meq param arg [("T", Unit)];
  let param = fnt "Option" [Unit]
  and arg = fnt "Option" [t] in
  meq param arg [("T", Unit)]

let suite =
  "Type matching" >::: [
      "Successful matches" >:: test_successful_matches
    ]

let _ = run_test_tt_main suite
