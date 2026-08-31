import fs from "node:fs";
import path from "node:path";

const root = process.cwd();
const required = [
  "app.R",
  "DESCRIPTION",
  "inst/sql/001_init.sql",
  "inst/app/www/app.js",
  "inst/app/www/styles.css"
];

for (const relative of required) {
  if (!fs.existsSync(path.join(root, relative))) {
    throw new Error(`Missing required file: ${relative}`);
  }
}

const rFiles = [];
function collect(directory) {
  for (const entry of fs.readdirSync(directory, { withFileTypes: true })) {
    const full = path.join(directory, entry.name);
    if (entry.isDirectory() && !entry.name.startsWith(".")) collect(full);
    if (entry.isFile() && entry.name.endsWith(".R")) rFiles.push(full);
  }
}
collect(root);

const pairs = { ")": "(", "]": "[", "}": "{" };
const openers = new Set(Object.values(pairs));

for (const file of rFiles) {
  const source = fs.readFileSync(file, "utf8");
  const stack = [];
  let quote = null;
  let escaped = false;
  let comment = false;

  for (let index = 0; index < source.length; index += 1) {
    const character = source[index];
    if (comment) {
      if (character === "\n") comment = false;
      continue;
    }
    if (quote) {
      if (escaped) escaped = false;
      else if (character === "\\") escaped = true;
      else if (character === quote) quote = null;
      continue;
    }
    if (character === "#") {
      comment = true;
      continue;
    }
    if (character === "\"" || character === "'" || character === "`") {
      quote = character;
      continue;
    }
    if (openers.has(character)) stack.push({ character, index });
    if (Object.hasOwn(pairs, character)) {
      const opened = stack.pop();
      if (!opened || opened.character !== pairs[character]) {
        throw new Error(`Unbalanced ${character} in ${path.relative(root, file)} at byte ${index}`);
      }
    }
  }

  if (quote) throw new Error(`Unclosed quote in ${path.relative(root, file)}`);
  if (stack.length) throw new Error(`Unclosed delimiter in ${path.relative(root, file)}`);
}

console.log(`Static structure check passed for ${rFiles.length} R files.`);
