"use server";

import { exec as execSync } from "child_process";
import { promisify } from "util";

const exec = promisify(execSync);

/**
 * Escapes special characters in a string.
 * @param str - The string to escape.
 * @returns The escaped string.
 */
function escapeChars(str: string): string {
    let escapedStr = str;
    const replacements: Array<[RegExp, string]> = [
        [/'/g, "\\'"],
        [/"/g, "\\\""],
        [/`/g, "\\`"],
    ];

    for (const [regex, replacement] of replacements)
        escapedStr = escapedStr.replace(regex, replacement);

    return escapedStr;
}

/**
 * Executes Haskell code.
 * @param code - The code to execute.
 * @returns The output of the code.
 */
export async function execute(code: string): Promise<string> {
    try {
        const cpuLimit = 0.5;
        const dockerCmd = `docker run --rm -m 128m --cpus=${cpuLimit} haskell:latest`;
        const bashCmd = `bash -c "echo '${escapeChars(code)}' > /tmp/script.hs && runghc /tmp/script.hs"`;

        return (await exec(`${dockerCmd} ${bashCmd}`)).stdout;
    } catch (err) {
        if (typeof err === "object" && err !== null && "stderr" in err)
            return String(err.stderr);
        // .trim().split("\n").slice(1).join("\n");

        return `Error: ${err instanceof Error ? err.message : String(err)}`;
    }
}
