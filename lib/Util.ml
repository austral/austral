let string_explode s =
  List.init (String.length s) (String.get s)

let string_implode l =
  String.init (List.length l) (List.nth l)
