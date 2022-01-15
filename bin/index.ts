import fs from "fs";
import util from "util";
import chalk from "chalk";

import { CowsLexer, transformSource, TransformerError } from "../src";
import path from "path/posix";

const fileName = path.join(process.cwd(), process.argv[2]);
const fileData = fs.readFileSync(fileName, "utf8");

const begin = Date.now();
const lexer = new CowsLexer(fileData, fileName);

while (lexer.advance()) {}

try {
    const transformed = transformSource(lexer.digest(), fileName);
    
    console.log(util.inspect(transformed, false, 10, true));

    console.log("Took %sms to compile", Date.now() - begin);
} catch (e) {
    if (e instanceof TransformerError) {
        let lineBeginPos = e.token.pos;
        while (fileData[lineBeginPos - 1] !== "\n" && lineBeginPos > 0) {
            lineBeginPos--;
        }
        let lineIdx = 1;
        for (let i = 0; i < lineBeginPos; i++) {
            if (fileData[i] === "\n") {
                lineIdx++;
            }
        }
        let lineEndPos = e.token.pos;
        while (fileData[lineEndPos + 1] !== "\n" && lineEndPos < fileData.length) {
            lineEndPos++;
        }
        const fullLine = fileData.substring(lineBeginPos, lineEndPos);
        const posRelativeToLine = e.token.pos - lineBeginPos;

        const lineText = " " + lineIdx.toString() + " ";

        const stdoutColumns = Math.min(process.stdout.columns, 70);
        const displayLineStart = Math.max(0, Math.floor(posRelativeToLine - (stdoutColumns / 2)));
        const displayLineEnd = Math.floor(posRelativeToLine + (stdoutColumns / 2));
        const displayLine = fullLine.substring(displayLineStart, displayLineEnd);

        const displayTokenArrowsStart = lineText.length + posRelativeToLine - displayLineStart - 4;
        
        console.log(`        Error encountered while compiling code`);
        console.log("");
        console.log(`${" ".repeat(lineText.length)}    ${chalk.cyanBright(path.relative(process.cwd(), fileName))}:${chalk.yellow(lineIdx)}:${chalk.yellow(posRelativeToLine)} - ${chalk.red("error: " + e.message)}`);
        console.log("");
        console.log(`${chalk.bgWhite(lineText)}    ${displayLine}`);
        console.log(`${chalk.bgWhite(" ".repeat(lineText.length))}    ${" ".repeat(displayTokenArrowsStart)}${chalk.red("~".repeat(e.token.end - e.token.pos + 3))}`);
    } else {
        throw e;
    }
}