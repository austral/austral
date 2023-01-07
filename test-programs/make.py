import os

types: list[str] = [
    "Nat8",
    "Int8",
    "Nat16",
    "Int16",
    "Nat32",
    "Int32",
    "Nat64",
    "Int64",
    "Index",
]
ops: list[str] = [
    "add",
    "sub",
    "mul",
    "div",
]

def code(typea: str, typeb: str) -> str:
    aconst: str = "10.0" if "Float" in typea else "10"
    bconst: str = "20.0" if "Float" in typeb else "20"
    return f"""
module body Test is
    function main() : ExitCode is
        let a: {a} := {aconst};
        let b: {b} := {bconst};
        let c: {a} := a * b;
        return ExitSuccess();
    end;
end module body.
    """

def errs(dirname, a,b):
    aconst: str = "10.0" if "Float" in a else "10"
    bconst: str = "20.0" if "Float" in b else "20"
    return f"""
Error:
  Title: Type Error
  Module:
    IntWrapper
  Location:
    Filename: 'test-programs/suites/012-numbers/{dirname}/Test.aum'
    From: line 3, column 8
    To: line 3, column 31
  Description:
    Expected a value of type `{a}` , but got a value of type `{b}`
  Code:
    1 | module body IntWrapper is
    2 |     function main() : ExitCode is
    3 |         let a: {a} := {aconst};
    4 |         let b: {b} := {bconst};
    5 |         let c: {a} := a * b;
"""

base: str = "suites/012-numbers/"

# Unordered cartesian product basically
for i, a in enumerate(types):
    for b in types[i:]:
        if a != b:
            for op in ops:
                name: str = f"{op}-{a}-{b}"
                os.makedirs(f"{base}/{name}")
                with open(f"{base}/{name}/Test.aum", "w") as sourcefile:
                    sourcefile.write(code(a,b))
                with open(f"{base}/{name}/austral-stderr.txt", "w") as errorfile:
                    errorfile.write(errs(name,a,b))
