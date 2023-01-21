type err_text = text_elem list

and text_elem =
  | Text of string
  | Code of string
  | Break

let rec error_text_to_plain (txt: err_text): string =
  String.concat "" (List.map text_elem_to_plain txt)

and text_elem_to_plain (elem: text_elem): string =
  match elem with
  | Text s ->
     s
  | Code c ->
     "`" ^ c ^ "`"
  | Break ->
     "\n\n"
