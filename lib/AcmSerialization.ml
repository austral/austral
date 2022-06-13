open Sexplib
open AcmFile
open Error

type sexp = Sexp.t

let fail (msg: string) =
  err ("ACM file parsing failed: " ^ msg)

let ser_acm_type_vis (vis: acm_type_vis): sexp =
  match vis with
  | AcmTypeVisPublic -> Atom "ACM_TYPE_VIS_PUBLIC"
  | AcmTypeVisOpaque -> Atom "ACM_TYPE_VIS_OPAQUE"

let par_acm_type_vis (sexp: sexp): acm_type_vis =
  match sexp with
  | Atom "ACM_TYPE_VIS_PUBLIC" -> AcmTypeVisPublic
  | Atom "ACM_TYPE_VIS_OPAQUE" -> AcmTypeVisOpaque
  | _ -> fail "bad acm_type_vis"
