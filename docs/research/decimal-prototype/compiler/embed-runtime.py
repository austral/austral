#!/usr/bin/env python3
"""Embeds the prototype Decimal runtime into lib/prelude.c of a compiler tree.

    embed-runtime.py <compiler-tree> <runtime-dir>

The compiler prepends only prelude.c to the C it generates, so the header and
the implementation both go there. Rerunning replaces the block between the markers.
"""
import pathlib
import re
import sys

BEGIN = "/* BEGIN zustral decimal prototype */"
END = "/* END zustral decimal prototype */"
WRITE_LINE = 'static void zd_write_line(const char *text) {\n  au_printf("%s\\n", text);\n}'

tree, runtime = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])
parts = [(runtime / "decimal.h").read_text().rstrip("\n"), WRITE_LINE, (runtime / "decimal.c").read_text().rstrip("\n")]
block = f"{BEGIN}\n" + "\n\n".join(parts) + f"\n{END}\n"

prelude = tree / "lib" / "prelude.c"
text = prelude.read_text()
existing = re.compile(re.escape(BEGIN) + r".*?" + re.escape(END) + r"\n", re.S)
text = existing.sub(lambda _: block, text) if existing.search(text) else text.rstrip("\n") + "\n\n" + block
prelude.write_text(text)
