"use client";

import { ContentCopy, CopyAll, Download, Image } from "@mui/icons-material";
import { Dialog, IconButton, Typography, useMediaQuery } from "@mui/material";
import { compressToEncodedURIComponent, decompressFromEncodedURIComponent } from "lz-string";
import { useContext, useEffect, useState } from "react";
import { Buttons } from "components/pages/editor/buttons";
import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import { NotificationsContext } from "contexts/notifications";
import type { ReactNode } from "react";
import { SplitView } from "components/pages/editor/splitView";
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
    const defaultCode = [
        "import Lib",
        "",
        "-- Start writing your code here.",
        "main :: IO ()",
        "main = render $ background LightGrey (createCanvas 800 600)",
        "",
    ].join("\n");
    const [code, setCode] = useLocalStorage("code", defaultCode);
    const [codeOutput, executeStream, terminateStream, clearStream] = useStreamAction(execute);
    const { setType, setMessage } = useContext(NotificationsContext);

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
        setType("success");
        setMessage("Code copied to clipboard.");
        setOpenShare(false);
    }) as () => void;
    const copyUrl = (async (): Promise<void> => {
        const codeURL = new URL(window.location.href);

        codeURL.searchParams.set("code", compressToEncodedURIComponent(code));
        await window.navigator.clipboard.writeText(codeURL.toString());
        setType("success");
        setMessage("URL copied to clipboard.");
        setOpenShare(false);
    }) as () => void;
    const copyImage = (): void => {
        void window.navigator.clipboard.write([
            new ClipboardItem({
                /* eslint-disable */ "image/png": new Promise(async (resolve) => { /* eslint-enable */
                    const canvas = document.querySelector("canvas")!;
                    const image = canvas.toDataURL("image/png");
                    const blob = await (await fetch(image)).blob();

                    resolve(new Blob([blob], { type: "image/png" }));
                }),
            }),
        ]).then(() => {
            setType("success");
            setMessage("Image copied to clipboard.");
            setOpenShare(false);
        });
    };
    const downloadImage = (): void => {
        const canvas = document.querySelector("canvas")!;
        const image = canvas.toDataURL("image/png");
        const a = document.createElement("a");

        a.href = image;
        a.download = "image.png";
        a.click();
        setType("success");
        setMessage("Image downloaded.");
        setOpenShare(false);
    };
    const stop = terminateStream;
    const run = ((): void => {
        stop();
        executeStream(code);
    }) as () => void;

    // Extract the graphics commands, and send them to the canvas.
    const graphicsRegEx = /drawToCanvas\((.*)\)/g;
    const graphics = codeOutput.join("").match(graphicsRegEx) ?? [];

    // Remove the graphics commands from the console output.
    const consoleOutput = codeOutput.join("")
        .split("\n")
        .map((output) => (output.startsWith("drawToCanvas(") ? "Compiling..." : output))
        .join("\n")
        .replace(/\n+/g, "\n");

    // Share options
    const shareOptions: Array<{ action: () => void; icon: ReactNode; label: string; }> = [
        { action: copyCode, icon: <ContentCopy />, label: "Copy Code" },
        { action: copyUrl, icon: <CopyAll />, label: "Copy URL" },
        { action: copyImage, icon: <Image />, label: "Copy Image" },
        { action: downloadImage, icon: <Download />, label: "Download Image" },
    ];

    return (
        <div className={`full-width ${styles.container}`}>
            <Dialog open={openShare} onClose={() => setOpenShare(false)}>
                <div className={styles.shareMenu}>
                    {shareOptions.map(({ action, icon, label }, i) => (
                        <div className={styles.shareOption} key={i}>
                            <IconButton onClick={action}>{icon}</IconButton>
                            <Typography>{label}</Typography>
                        </div>
                    ))}
                </div>
            </Dialog>
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
