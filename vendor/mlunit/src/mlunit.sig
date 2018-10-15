signature ML_UNIT = sig
  (* Basic definitions *)

  datatype result = Pass
                  | Fail of string
  type testable

  val suite : string -> testable list -> testable

  (* Assertions *)

  val is : (unit -> result) -> string -> testable

  val isTrue : bool -> string -> testable
  val isTrue' : bool -> testable

  val isFalse : bool -> string -> testable
  val isFalse' : bool -> testable

  val isEqual : ''a -> ''a -> string -> testable
  val isEqual' : ''a -> ''a -> testable

  val isNotEqual : ''a -> ''a -> string -> testable
  val isNotEqual' : ''a -> ''a -> testable

  val isSome : 'a option -> string -> testable
  val isSome' : 'a option -> testable

  val isNone : 'a option -> string -> testable
  val isNone' : 'a option -> testable

  val isEmpty : 'a list -> string -> testable
  val isEmpty' : 'a list -> testable

  val isNonEmpty : 'a list -> string -> testable
  val isNonEmpty' : 'a list -> testable

  (* Running *)

  type testable_result

  val run : testable -> testable_result

  (* Statistics *)

  val count_tests : testable_result -> int
  val count_passed : testable_result -> int
  val count_failed : testable_result -> int

  (* Reporting *)

  type reporter = testable_result -> string

  val defaultReporter : reporter

  val runAndPrint : testable -> reporter -> unit
  val runAndQuit : testable -> reporter -> unit
end
