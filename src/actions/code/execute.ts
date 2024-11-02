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
    await Promise.resolve();

    try {
        const { stdout, stderr } = await exec(`echo '${code}' | runghc`);

        return stderr || stdout;
    } catch (err) {
        return `Error: ${err instanceof Error ? err.message : String(err)}`;
    }
}
