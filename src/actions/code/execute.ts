"use server";

import { exec as execSync } from "child_process";
import { promisify } from "util";

const exec = promisify(execSync);

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
 * Executes Haskell code.
 * @param code - The code to execute.
 * @returns The output of the code.
 */
export async function execute(code: string): Promise<string> {
    try {
        const cpuLimit = 0.5;
        const dockerCmd = `docker run --rm -m 128m --cpus=${cpuLimit} haskell:latest`;
        // eslint-disable-next-line max-len
        const bashCmd = `bash -c "echo '${escapeChars(code)}\n\n${escapeChars(lib)}' > /tmp/script.hs && runghc /tmp/script.hs"`;

        return (await exec(`${dockerCmd} ${bashCmd}`)).stdout;
    } catch (err) {
        if (typeof err === "object" && err !== null && "stderr" in err)
            return String(err.stderr);
        // .trim().split("\n").slice(1).join("\n");

        return `Error: ${err instanceof Error ? err.message : String(err)}`;
    }
}
