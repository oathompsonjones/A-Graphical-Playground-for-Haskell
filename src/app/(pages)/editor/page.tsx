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

/**
 * This is the editor page.
 * @returns The page element.
 */
export default function EditorPage(): ReactNode {
    const isPortrait = useMediaQuery("(orientation: portrait)");
    const defaultCode = "-- Start writing your code here.\n\n";
    const [code, setCode] = useLocalStorage("code", defaultCode);
    const [consoleOutput, setConsoleOutput] = useState("");
    const [openShare, setOpenShare] = useState(true);

    const new_ = (): void => {
        setConsoleOutput("");
        setCode(defaultCode);
    };
    const open = (): void => undefined;
    const save = (): void => undefined;
    const share = (async (): Promise<void> => {
        await window.navigator.clipboard.writeText(code);
        setOpenShare(true);
    }) as () => void;
    const stop = (): void => undefined;
    const run = (async (): Promise<void> => setConsoleOutput(await execute(code))) as () => void;

    return (
        <div className={`full-width ${styles.container}`}>
            <Modal open={openShare} onClose={() => setOpenShare(false)}>
                <Paper className={styles.shareMenu!}>
                    <Typography variant="h6">
                        The code has been copied to the clipboard.
                    </Typography>
                </Paper>
            </Modal>
            <Buttons new={new_} open={open} save={save} share={share} stop={stop} run={run} />
            <SplitView vertical={isPortrait} id="editor-horizontal">
                <SplitView vertical id="editor-vertical">
                    <Editor code={code} updateCode={setCode} />
                    <Console content={consoleOutput} />
                </SplitView>
                <Canvas />
            </SplitView>
        </div>
    );
}
