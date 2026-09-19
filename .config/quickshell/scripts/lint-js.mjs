import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { analyzeFileComplexity } from "oxlint-plugin-complexity/standalone";

const root = fileURLToPath(new URL("../", import.meta.url));
const config = JSON.parse(fs.readFileSync(path.join(root, ".oxlintrc.json"), "utf8"));
const limits = config.rules["complexity/complexity"][1];

function sourceFiles(relative) {
    const filename = path.join(root, relative);
    if (!fs.statSync(filename).isDirectory())
        return /\.(?:[cm]?js|[cm]?ts|tsx|jsx)$/.test(relative) ? [relative] : [];
    return fs.readdirSync(filename).sort().flatMap(name => sourceFiles(path.join(relative, name)));
}

function analyze(filename) {
    // Qt's .pragma/.import directives are not ECMAScript. Preserve line numbers
    // while analyzing the remaining source with the configured oxlint plugin.
    const source = fs.readFileSync(path.join(root, filename), "utf8")
        .replace(/^\.(?:pragma|import)[^\n]*/gm, "");
    return analyzeFileComplexity(source, filename);
}

function exceedsLimits(fn) {
    return fn.cyclomatic > limits.cyclomatic || fn.cognitive > limits.cognitive;
}

const json = process.argv.includes("--json");
const requested = process.argv.slice(2).filter(arg => arg !== "--json");
const files = (requested.length ? requested : ["Common", "scripts", "tests", "research"]).flatMap(sourceFiles);
const results = files.map(analyze);
const over = results.flatMap(file => file.functions.filter(exceedsLimits).map(fn => ({ file: file.filename, ...fn })));

if (json) {
    console.log(JSON.stringify(results, null, 2));
} else {
    for (const fn of over)
        console.log(`${fn.file}:${fn.startLine} ${fn.name}: cyclomatic ${fn.cyclomatic}/${limits.cyclomatic}, cognitive ${fn.cognitive}/${limits.cognitive}`);
    console.log(`${files.length} files, ${over.length} functions exceed the complexity limits.`);
}
process.exitCode = over.length ? 1 : 0;
