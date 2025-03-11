"use server";

import { readFile, readdir } from "fs/promises";
import { exec } from "child_process";

type Response = {
    status: "done" | "error" | "running";
    data: string;
};

/**
 * Executes Haskell code, streaming the response into a ReadableStream.
 * @param code - The code to execute.
 * @returns A ReadableStream containing the output of the code.
 */
export async function execute(code: string): Promise<ReadableStream<Response>> {
    try {
        const base64 = (str: string): string => Buffer.from(str).toString("base64");
        const files = await readdir("public/lib");
        const lib = await Promise.all(files.map(async (name) => ({
            content: await readFile(`public/lib/${name}`, "utf8"),
            name,
        })));

        const dockerCmd = "docker run --rm -m 128m --cpus=0.5 haskell:latest";
        const bashCmd = [
            "cd /tmp",
            ...lib.map(({ content, name }) => `base64 -d <<< ${base64(content)} > ${name}`),
            `base64 -d <<< ${base64(code)} > main.hs`,
            "runhaskell main.hs",
        ].join(" && ");

        const timeout = 150_000;
        const updateDelay = 0.1;
        const stopDelay = 2_500;

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

                limitTimer = setTimeout(timeoutStream, timeout);

                updateStream();
                updateInterval = setInterval(updateStream, updateDelay);

                if (stream.stdout === null || stream.stderr === null)
                    throw new Error("Stream stdout or stderr is null");

                stream.stdout.on("data", (data) => updateQueue.push(String(data)));
                stream.stderr.on("data", (data) => updateQueue.push(String(data)));
                stream.on("error", (error) => {
                    controller.enqueue({ data: error.message, status: "error" });
                    handleStreamEnd();
                });

                /** Updates the stream with new data. */
                function updateStream(): void {
                    const newData: string | undefined = updateQueue.shift();

                    if (newData === undefined) {
                        if (Date.now() - timeOfLastNewData > stopDelay) {
                            controller.enqueue({ data: "IDLE", status: "done" });
                            handleStreamEnd();
                        }

                        return;
                    }

                    timeOfLastNewData = Date.now();
                    controller.enqueue({ data: newData, status: "running" });
                }

                /** Handles the timeout of the stream. */
                function timeoutStream(): void {
                    controller.enqueue({ data: "TIMEOUT", status: "done" });
                    handleStreamEnd();
                }

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
                controller.enqueue({
                    data: typeof err === "object" && err !== null && "stderr" in err
                        ? String(err.stderr)
                        : `Error: ${err instanceof Error ? err.message : String(err)}`,
                    status: "error",
                });
                controller.close();
            },
        });
    }
}
