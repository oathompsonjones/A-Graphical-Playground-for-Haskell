"use client";

import { Alert, IconButton, Modal, Paper, Typography, useMediaQuery } from "@mui/material";
import { ContentCopy, CopyAll } from "@mui/icons-material";
import { compressToEncodedURIComponent, decompressFromEncodedURIComponent } from "lz-string";
import { useEffect, useState } from "react";
import { Buttons } from "components/pages/editor/buttons";
import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import type { ReactNode } from "react";
import { SplitView } from "components/splitView";
import { execute } from "actions/code/execute";
import styles from "styles/pages/editor.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";
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
    const [alert, setAlert] = useState<string | null>(null);

    useEffect(() => {
        const url = new URL(window.location.href);
        const codeParam = url.searchParams.get("code");

        if (codeParam !== null)
            setCode(decompressFromEncodedURIComponent(codeParam));
    }, []);

    const clear = clearStream;
    const new_ = (): void => {
        clearStream();
        setCode(defaultCode);
    };
    const open = (): void => undefined;
    const save = (): void => undefined;
    const share = (): void => setOpenShare(true);
    const copyCode = (async (): Promise<void> => {
        await window.navigator.clipboard.writeText(code);
        setAlert("Code copied to clipboard.");
    }) as () => void;
    const copyUrl = (async (): Promise<void> => {
        const codeURL = new URL(window.location.href);

        codeURL.searchParams.set("code", compressToEncodedURIComponent(code));
        await window.navigator.clipboard.writeText(codeURL.toString());
        setAlert("URL copied to clipboard.");
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

    // Share options
    const shareOptions: Array<{ action: () => void; icon: ReactNode; label: string; }> = [
        { action: copyCode, icon: <ContentCopy />, label: "Copy Code" },
        { action: copyUrl, icon: <CopyAll />, label: "Copy URL" },
    ];

    return (
        <div className={`full-width ${styles.container}`}>
            <Modal open={openShare} onClose={() => setOpenShare(false)}>
                <Paper className={styles.shareMenuWrapper!}>
                    {alert !== null && <Alert severity="success">{alert}</Alert>}
                    <div className={styles.shareMenu}>
                        {shareOptions.map(({ action, icon, label }, i) => (
                            <div className={styles.shareOption} key={i}>
                                <IconButton onClick={action}>{icon}</IconButton>
                                <Typography>{label}</Typography>
                            </div>
                        ))}
                    </div>
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
