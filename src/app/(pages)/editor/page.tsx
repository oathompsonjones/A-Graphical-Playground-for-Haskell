"use client";

import { Modal, Paper, Typography, useMediaQuery } from "@mui/material";
import { Buttons } from "components/pages/editor/buttons";
import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import type { ReactNode } from "react";
import { SplitView } from "components/splitView";
import { execute } from "actions/code/execute";
import styles from "styles/pages/editor.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";
import { useState } from "react";
import { useStreamAction } from "hooks/useStreamAction";

/**
 * This is the editor page.
 * @returns The page element.
 */
export default function EditorPage(): ReactNode {
    const isPortrait = useMediaQuery("(orientation: portrait)");
    const [openShare, setOpenShare] = useState(false);
    const defaultCode = "-- Start writing your code here.\n\n";
    const [code, setCode] = useLocalStorage("code", defaultCode);
    const [codeOutput, executeStream, terminateStream, clearStream] = useStreamAction(execute);

    const clear = clearStream;
    const new_ = (): void => {
        clearStream();
        setCode(defaultCode);
    };
    const open = (): void => undefined;
    const save = (): void => undefined;
    const share = (async (): Promise<void> => {
        await window.navigator.clipboard.writeText(code);
        setOpenShare(true);
    }) as () => void;
    const stop = terminateStream;
    const run = ((): void => {
        // Stop any currently executing code.
        stop();

        // Execute the code.
        executeStream(code);
    }) as () => void;

    // Extract the graphics commands, and send them to the canvas.
    const graphicsRegEx = /drawToCanvas\((.*)\)/g;
    const graphics = codeOutput.join("").match(graphicsRegEx) ?? [];

    // Remove the graphics commands from the console output.
    const consoleOutput = codeOutput.join("")
        .split("\n")
        .map((output) => output.replace(graphicsRegEx, ""))
        .join("\n")
        .replace(/\n+/g, "\n");

    return (
        <div className={`full-width ${styles.container}`}>
            <Modal open={openShare} onClose={() => setOpenShare(false)}>
                <Paper className={styles.shareMenu!}>
                    <Typography variant="h6">
                        The code has been copied to the clipboard.
                    </Typography>
                </Paper>
            </Modal>
            <Buttons
                title="untitled"
                new={new_}
                clear={clear}
                open={open}
                save={save}
                share={share}
                stop={stop}
                run={run}
            />
            <SplitView vertical={isPortrait} id="editor-horizontal">
                <SplitView vertical id="editor-vertical">
                    <Editor code={code} updateCode={setCode} save={save} run={run} />
                    <Console content={consoleOutput} />
                </SplitView>
                <Canvas content={graphics} />
            </SplitView>
        </div>
    );
}
