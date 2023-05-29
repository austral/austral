import grammar from "./austral.tmlanguage";
import { writeFile } from "fs";
import * as path from "path";
const syntaxPath = path.join(
  __dirname,
  "..",
  "syntaxes",
  "austral.tmLanguage.json"
);

writeFile(syntaxPath, JSON.stringify(grammar), (err) => {
  if (err) console.error(`failed to write syntax: ${err}`);
});
