import grammar from "./austral.tmLanguage";
import { writeFile } from "fs";
import * as path from "path";
const syntaxPath = path.join(
  __dirname,
  "..",
  "syntaxes",
  "austral.tmLanguage.json"
);

writeFile(syntaxPath, JSON.stringify(grammar, undefined, 4), (err) => {
  if (err) console.error(`failed to write syntax: ${err}`);
});
