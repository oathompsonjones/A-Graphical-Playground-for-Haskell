"use client";

import { Button, Dialog, FormControl, IconButton, TextField, Typography, useMediaQuery } from "@mui/material";
import { ContentCopy, CopyAll, Download, Image } from "@mui/icons-material";
import { compressToEncodedURIComponent, decompressFromEncodedURIComponent } from "lz-string";
import { useContext, useEffect, useState } from "react";
import { Buttons } from "components/pages/editor/buttons";
import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import { NotificationsContext } from "contexts/notifications";
import type { ReactNode } from "react";
import type { Sketch } from "schemas/database";
import { SplitView } from "components/pages/editor/splitView";
import { UserContext } from "contexts/user";
import { execute } from "actions/code/execute";
import { getSketches } from "database/index";
import { saveSketch } from "actions/code/saveSketch";
import styles from "styles/pages/editor.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";
import { useOutsideClick } from "hooks/useOutsideClick";
import { useStreamAction } from "hooks/useStreamAction";

/**
 * This is the editor page.
 * @returns The page element.
 */
// eslint-disable-next-line max-statements, max-lines-per-function
export default function EditorPage(): ReactNode {
    const { user } = useContext(UserContext);
    const [sketches, setSketches] = useState<Sketch[] | null>(null);
    const isPortrait = useMediaQuery("(orientation: portrait)");
    const [openShare, setOpenShare] = useState(false);
    const shareRef = useOutsideClick<HTMLDivElement>(() => setOpenShare(false));
    const [openOpen, setOpenOpen] = useState(false);
    const openRef = useOutsideClick<HTMLDivElement>(() => setOpenOpen(false));
    const [openSave, setOpenSave] = useState(false);
    const saveRef = useOutsideClick<HTMLDivElement>(() => setOpenSave(false));
    const defaultTitle = "untitled";
    const defaultCode = compressToEncodedURIComponent([
        "import Lib",
        "",
        "-- Start writing your code here.",
        "main :: IO ()",
        "main = render $ background LightGrey (createCanvas 800 600)",
        "",
    ].join("\n"));
    const [title, setTitle] = useLocalStorage("title", defaultTitle);
    const [code, setCode] = useLocalStorage("code", defaultCode);
    const updateCode = (rawCode: string): void => setCode(compressToEncodedURIComponent(rawCode));
    const [author, setAuthor] = useState<string | null>(user === null
        ? null
        : user.username ?? user.email.split("@")[0]!);
    const [codeOutput, executeStream, terminateStream, clearStream] = useStreamAction(execute);
    const { setType, setMessage } = useContext(NotificationsContext);

    const fetchSketches = (): void => {
        if (user && sketches === null) {
            getSketches(user._id.toString())
                .then((json) => setSketches(JSON.parse(json) as Sketch[]))
                .catch(() => undefined);
        }
    };

    const onSketchClick = (sketch: Sketch): void => {
        window.location.search = `code=${sketch.content}&title=${sketch.name}&author=${author}`;
    };

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

        fetchSketches();
    }, []);

    const clear = clearStream;
    const new_ = (): void => {
        clearStream();
        setTitle(defaultTitle);
        setCode(defaultCode);
    };
    const open = (): void => setOpenOpen(true);
    const save = (): void => setOpenSave(true);
    const share = (): void => setOpenShare(true);
    const copyCode = (): void => {
        void window.navigator.clipboard.writeText(decompressFromEncodedURIComponent(code)).then(() => {
            setType("success");
            setMessage("Code copied to clipboard.");
            setOpenShare(false);
        });
    };
    const copyUrl = (): void => {
        const codeURL = new URL(window.location.href);

        codeURL.searchParams.set("code", code);

        if (title !== defaultTitle)
            codeURL.searchParams.set("title", encodeURIComponent(title));

        if (author !== null)
            codeURL.searchParams.set("author", author);

        void window.navigator.clipboard.writeText(codeURL.toString()).then(() => {
            setType("success");
            setMessage("URL copied to clipboard.");
            setOpenShare(false);
        });
    };
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

    // Share options
    const shareOptions: Array<{ action: () => void; icon: ReactNode; label: string; }> = [
        { action: copyCode, icon: <ContentCopy />, label: "Copy Code" },
        { action: copyUrl, icon: <CopyAll />, label: "Copy URL" },
        { action: copyImage, icon: <Image />, label: "Copy Image" },
        { action: downloadImage, icon: <Download />, label: "Download Image" },
    ];

    const saveSketchAction = (formData: FormData): void => {
        saveSketch(formData).then(() => {
            setOpenSave(false);
            setType("success");
            setMessage("Sketch saved.");
            fetchSketches();
        }).catch((e: unknown) => {
            setType("error");
            setMessage(e instanceof Error ? e.message : "An error occurred.");
        });
    };

    return (
        <div className={`full-width ${styles.container}`}>
            <Dialog open={openShare} onClose={() => setOpenShare(false)} ref={shareRef}>
                <div className={styles.shareDialog}>
                    {shareOptions.map(({ action, icon, label }, i) => (
                        <div className={styles.shareOption} key={i}>
                            <IconButton onClick={action}>{icon}</IconButton>
                            <Typography>{label}</Typography>
                        </div>
                    ))}
                </div>
            </Dialog>
            <Dialog open={openOpen} onClose={() => setOpenOpen(false)} ref={openRef}>
                {user !== null && <div className={styles.openSaveDialog}>
                    {sketches === null || sketches.length === 0
                        ? <Typography>You have no saved sketches.</Typography>
                        : <>
                            <Typography variant="h6">Your Sketches</Typography>
                            {sketches.map((sketch, i) => (
                                <div key={i}>
                                    <a onClick={() => onSketchClick(sketch)}>{sketch.name}</a>
                                </div>
                            ))}
                        </>}
                </div>}
            </Dialog>
            <Dialog open={openSave} onClose={() => setOpenSave(false)} ref={saveRef}>
                {user !== null && <div className={styles.openSaveDialog}>
                    <FormControl
                        action={saveSketchAction}
                        component="form">
                        <TextField type="hidden" name="content" value={code} sx={{ opacity: 0 }} />
                        <TextField type="hidden" name="authorId" value={user._id.toString()} sx={{ opacity: 0 }} />
                        <TextField label="Name" name="name" />
                        <br />
                        <Button type="submit">Save</Button>
                    </FormControl>
                </div>}
            </Dialog>
            <Buttons
                title={title}
                new={new_}
                clear={clear}
                open={open}
                save={save}
                share={share}
                stop={stop}
                run={run}
                loggedIn={user !== null}
                author={author}
            />
            <SplitView vertical={isPortrait} id="editor-horizontal">
                <SplitView vertical id="editor-vertical">
                    <Editor
                        code={decompressFromEncodedURIComponent(code)}
                        updateCode={updateCode}
                        save={save}
                        open={open}
                        new={new_}
                        run={run}
                    />
                    <Console content={consoleOutput} />
                </SplitView>
                <Canvas content={graphics} />
            </SplitView>
        </div>
    );
}
