"use server";

import { exec } from "child_process";

/* eslint-disable max-len */
const lib = `drawToCanvas :: String -> IO ()
drawToCanvas x = putStrLn $ "drawToCanvas(" ++ x ++ ")"

-- Setting

background :: Float -> Float -> Float -> IO ()
background r g b = drawToCanvas $ "background(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

fill :: Float -> Float -> Float -> IO ()
fill r g b = drawToCanvas $ "fill(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

stroke :: Float -> Float -> Float -> IO ()
stroke r g b = drawToCanvas $ "stroke(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

strokeWeight :: Float -> IO ()
strokeWeight w = drawToCanvas $ "strokeWeight(" ++ show w ++ ")"

noStroke :: IO ()
noStroke = drawToCanvas "noStroke()"

noFill :: IO ()
noFill = drawToCanvas "noFill()"

-- 2D Primitives

circle :: Float -> Float -> Float -> IO ()
circle x y d = drawToCanvas $ "circle(" ++ show x ++ ", " ++ show y ++ ", " ++ show d ++ ")"

ellipse :: Float -> Float -> Float -> Float -> IO ()
ellipse x y w h = drawToCanvas $ "ellipse(" ++ show x ++ ", " ++ show y ++ ", " ++ show w ++ ", " ++ show h ++ ")"

line :: Float -> Float -> Float -> Float -> IO ()
line x1 y1 x2 y2 = drawToCanvas $ "line(" ++ show x1 ++ ", " ++ show y1 ++ ", " ++ show x2 ++ ", " ++ show y2 ++ ")"

point :: Float -> Float -> IO ()
point x y = drawToCanvas $ "point(" ++ show x ++ ", " ++ show y ++ ")"

quad :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()
quad x1 y1 x2 y2 x3 y3 x4 y4 = drawToCanvas $ "quad(" ++ show x1 ++ ", " ++ show y1 ++ ", " ++ show x2 ++ ", " ++ show y2 ++ ", " ++ show x3 ++ ", " ++ show y3 ++ ", " ++ show x4 ++ ", " ++ show y4 ++ ")"

rect :: Float -> Float -> Float -> Float -> IO ()
rect x y w h = drawToCanvas $ "rect(" ++ show x ++ ", " ++ show y ++ ", " ++ show w ++ ", " ++ show h ++ ")"

square :: Float -> Float -> Float -> IO ()
square x y s = drawToCanvas $ "square(" ++ show x ++ ", " ++ show y ++ ", " ++ show s ++ ")"

triangle :: Float -> Float -> Float -> Float -> Float -> Float -> IO ()
triangle x1 y1 x2 y2 x3 y3 = drawToCanvas $ "triangle(" ++ show x1 ++ ", " ++ show y1 ++ ", " ++ show x2 ++ ", " ++ show y2 ++ ", " ++ show x3 ++ ", " ++ show y3 ++ ")"
`;
/* eslint-enable max-len */

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
    await Promise.resolve();

    try {
        // ! const cpuLimit = 0.5; const dockerCmd = `docker run --rm -m 128m --cpus=${cpuLimit} haskell:latest`;
        const bashCmd = `echo '${escapeChars(code)}\n\n${escapeChars(lib)}' > /tmp/script.hs && runghc /tmp/script.hs`;

        const timeout = 3_600_000;
        const updateDelay = 10;
        const stopDelay = 1_000;

        const stream = exec(`bash -c "${bashCmd}"`, { timeout });
        let limitTimer: NodeJS.Timeout;
        let updateInterval: NodeJS.Timeout;

        // TODO: consider adding back in the queue using an interval to update the stream, may be less blocking

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
