import { build } from "esbuild";
import { readFile, writeFile, mkdir, readdir } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const directory = path.dirname(fileURLToPath(import.meta.url));
const output = path.resolve(directory, "../../inst/defuddle");
await mkdir(output, { recursive: true });
const result = await build({
  absWorkingDir: directory,
  entryPoints: ["node_modules/defuddle/dist/cli.js"],
  outfile: path.join(output, "defuddle.cjs"),
  bundle: true,
  platform: "node",
  format: "cjs",
  target: "node18",
  external: ["canvas"],
  minify: true,
  legalComments: "eof",
  metafile: true,
});

const packageDirectories = [...new Set(Object.keys(result.metafile.inputs)
  .filter((file) => file.includes("node_modules/"))
  .map((file) => file.match(/^(.*node_modules\/(?:@[^/]+\/)?[^/]+)/)[1]))]
  .sort();
const notices = [];
for (const relative of packageDirectories) {
  const location = path.join(directory, relative);
  const metadata = JSON.parse(await readFile(path.join(location, "package.json"), "utf8"));
  const licenseFiles = (await readdir(location)).filter((name) => /^licen[cs]e(?:\.|$)/i.test(name));
  if (!licenseFiles.length) throw new Error(`Missing license for ${metadata.name}`);
  const licenses = await Promise.all(licenseFiles.sort().map((name) => readFile(path.join(location, name), "utf8")));
  notices.push(`${metadata.name} ${metadata.version} (${metadata.license})\n${licenses.join("\n")}`);
}
await writeFile(path.join(output, "LICENSES.txt"), notices.join("\n\n---\n\n"));
const defuddle = JSON.parse(await readFile(path.join(directory, "node_modules/defuddle/package.json"), "utf8"));
await writeFile(path.join(output, "version.txt"), `${defuddle.version}\n`);
