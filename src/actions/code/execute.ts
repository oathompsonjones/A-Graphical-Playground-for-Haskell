"use server";

import { exec } from "child_process";
import { readFile } from "fs/promises";

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
 * Executes Haskell code, streaming the response into a ReadableStream.
 * @param code - The code to execute.
 * @returns A ReadableStream containing the output of the code.
 */
export async function execute(code: string): Promise<ReadableStream<string>> {
    try {
        const lib = await readFile("public/lib.hs", "utf8");
        const cpuLimit = 0.5;
        const dockerCmd = `docker run --rm -m 128m --cpus=${cpuLimit} haskell:latest`;
        const bashCmd = `echo '${escapeChars(lib)}\n\n${escapeChars(code)}' > /tmp/script.hs && runghc /tmp/script.hs`;

        const timeout = 3_600_000;
        const updateDelay = 10;
        const stopDelay = 5_000;

        const stream = exec(`${dockerCmd} bash -c "${bashCmd}"`, { timeout });
        let limitTimer: NodeJS.Timeout;
        let updateInterval: NodeJS.Timeout;

        const cancel = (): void => {
            stream.kill();
            clearTimeout(limitTimer);
            clearInterval(updateInterval);
        };

        return new ReadableStream({
            cancel,
            start(controller): () => void {
                const updateQueue: string[] = [];
                let timeOfLastNewData = Date.now();

                limitTimer = setTimeout(() => {
                    controller.enqueue("\n\nTIMEOUT");
                    handleStreamEnd();
                }, timeout);

                updateInterval = setInterval(() => {
                    const newData: string | undefined = updateQueue.shift();

                    if (newData === undefined) {
                        if (Date.now() - timeOfLastNewData > stopDelay) {
                            controller.enqueue(`\n\nNo data received for ${stopDelay}ms, listener killed`);
                            handleStreamEnd();
                        }

                        return;
                    }

                    timeOfLastNewData = Date.now();
                    controller.enqueue(newData);

                    if (updateQueue.length === 0)
                        handleStreamEnd();
                }, updateDelay);

                if (stream.stdout === null || stream.stderr === null)
                    throw new Error("Stream stdout or stderr is null");

                stream.stdout.on("data", (data) => updateQueue.push(String(data)));
                stream.stderr.on("data", (data) => updateQueue.push(String(data)));
                stream.on("error", (error) => {
                    controller.enqueue(error.message);
                    handleStreamEnd();
                });

                /** Handles the end of the stream. */
                function handleStreamEnd(): void {
                    cancel();
                    controller.close();
                }

                return handleStreamEnd;
            },
        });
    } catch (err) {
        return new ReadableStream({
            start(controller): void {
                controller.enqueue(
                    typeof err === "object" && err !== null && "stderr" in err
                        ? String(err.stderr)
                        : `Error: ${err instanceof Error ? err.message : String(err)}`,
                );
                controller.close();
            },
        });
    }
}
