"use client";

import { compressToEncodedURIComponent, decompressFromEncodedURIComponent } from "lz-string";
import { useContext, useEffect, useState } from "react";
import { Buttons } from "components/pages/editor/buttons";
import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import { OpenMenu } from "components/pages/editor/openMenu";
import type { ReactNode } from "react";
import { SaveMenu } from "components/pages/editor/saveMenu";
import { ShareMenu } from "components/pages/editor/shareMenu";
import { SplitView } from "components/pages/editor/splitView";
import { UserContext } from "contexts/user";
import { execute } from "actions/code/execute";
import styles from "styles/pages/editor.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";
import { useMediaQuery } from "@mui/material";
import { useStreamAction } from "hooks/useStreamAction";

/**
 * This is the editor page.
 * @returns The page element.
 */
export default function EditorPage(): ReactNode {
    const { user } = useContext(UserContext);
    const isPortrait = useMediaQuery("(orientation: portrait)");
    const [openShare, setOpenShare] = useState(false);
    const [openOpen, setOpenOpen] = useState(false);
    const [openSave, setOpenSave] = useState(false);
    const [title, setTitle, resetTitle] = useLocalStorage("title", "untitled");
    const [code, setCode, resetCode] = useLocalStorage("code", compressToEncodedURIComponent([
        "import Lib",
        "",
        "-- Start writing your code here.",
        "main :: IO ()",
        "main = render $ background LightGrey (createCanvas 800 600)",
        "",
    ].join("\n")));
    const updateCode = (rawCode: string): void => setCode(compressToEncodedURIComponent(rawCode));
    const [author, setAuthor] = useState<string | null>(user === null
        ? null
        : user.username ?? user.email.split("@")[0]!);
    const [codeOutput, executeStream, terminateStream, clearStream] = useStreamAction(execute);

    useEffect(() => {
        const url = new URL(window.location.href);
        const codeParam = url.searchParams.get("code");
        const titleParam = url.searchParams.get("title");
        const authorParam = url.searchParams.get("author");

        if (codeParam !== null)
            setCode(codeParam);

        if (titleParam !== null)
            setTitle(decodeURIComponent(titleParam));

        if (authorParam !== null)
            setAuthor(authorParam);
    }, []);

    const clear = clearStream;
    const new_ = (): void => {
        clearStream();
        resetTitle();
        resetCode();
        setAuthor(user?.username ?? user?.email.split("@")[0] ?? null);
    };
    const open = (): void => setOpenOpen(true);
    const save = (): void => setOpenSave(true);
    const share = (): void => setOpenShare(true);
    const stop = terminateStream;
    const run = (): void => {
        stop();
        executeStream(decompressFromEncodedURIComponent(code));
    };

    // Extract the graphics commands, and send them to the canvas.
    const graphicsRegEx = /drawToCanvas\((.*)\)/g;
    const graphics = codeOutput.join("").match(graphicsRegEx) ?? [];

    // Remove the graphics commands from the console output.
    const consoleOutput = codeOutput.join("")
        .split("\n")
        .map((output) => (output.startsWith("drawToCanvas(") ? "Compiling..." : output))
        .join("\n")
        .replace(/\n+/g, "\n");

    return (
        <div className={`full-width ${styles.container}`}>
            <ShareMenu open={openShare} setOpen={setOpenShare} code={code} title={title} author={author} />
            <OpenMenu open={openOpen} setOpen={setOpenOpen} author={author} />
            <SaveMenu open={openSave} setOpen={setOpenSave} code={code} />
            <Buttons
                new={new_} clear={clear} open={open} save={save} share={share} stop={stop} run={run}
                loggedIn={user !== null} author={author} title={title}
            />
            <SplitView vertical={isPortrait} id="editor-horizontal">
                <SplitView vertical id="editor-vertical">
                    <Editor
                        code={decompressFromEncodedURIComponent(code)}
                        updateCode={updateCode} save={save} open={open} new={new_} run={run}
                    />
                    <Console content={consoleOutput} />
                </SplitView>
                <Canvas content={graphics} />
            </SplitView>
        </div>
    );
}
