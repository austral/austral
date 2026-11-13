Pins which names get the reserved-word escape (`au_ckw_`) in the generated C. Every name in `forms.aum` starts with a C reserved word. This runner only sees that the compiler succeeds; the generated C is compared by `tools/refactoring/equivalence.py`. The C does not compile, which is why the target is C. What the compiler does today, all of it tracked in issue #92:

- Variables, parameters and loop variables are escaped. The escape fires on any name that starts with a reserved word (`integer`, `structure`), not only on a reserved word.
- Record slots and union cases are not escaped: `au_int64_t double;`, `struct {} default;`.
- A constant is defined without the escape and used with it when its module's name starts with a reserved word: `#define forms____integer 7`, then `au_ckw_forms____integer`.
- The name in `External_Name` of `Foreign_Import` is declared without the escape and called with it: `extern au_int32_t fork(au_int32_t intArg);`, then `au_ckw_fork(au_ckw_intArg)`. The parameters of that `extern` declaration are not escaped either.
- The name in `External_Name` of `Foreign_Export` is escaped: the wrapper is `au_ckw_double_it`, not `double_it`.
