"use server";

import { exec as execSync } from "child_process";
import { promisify } from "util";

const exec = promisify(execSync);

/**
 * Executes Haskell code.
 * @param code - The code to execute.
 * @returns The output of the code.
 */
export async function execute(code: string): Promise<string> {
    try {
        return (await exec(`echo "${code.replace(/"/g, "\\\"")}" | runghc`)).stdout;
    } catch (err) {
        console.log(err);

        if (typeof err === "object" && err !== null && "stderr" in err) {
            return String(err.stderr)
                .split("\n")
                .slice(1)
                .join("\n");
        }

        return `Error: ${err instanceof Error ? err.message : String(err)}`;
    }
}
