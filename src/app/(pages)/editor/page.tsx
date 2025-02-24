"use client";

import { Typography, useMediaQuery } from "@mui/material";
import { compressToEncodedURIComponent, decompressFromEncodedURIComponent } from "lz-string";
import { useContext, useEffect, useState } from "react";
import { Buttons } from "components/pages/editor/buttons";
import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import { NewWarningMenu } from "components/pages/editor/menus/newWarningMenu";
import { NotificationsContext } from "contexts/notifications";
import { OpenMenu } from "components/pages/editor/menus/openMenu";
import { OpenWarningMenu } from "components/pages/editor/menus/openWarningMenu";
import type { ReactNode } from "react";
import { SaveMenu } from "components/pages/editor/menus/saveMenu";
import { ShareMenu } from "components/pages/editor/menus/shareMenu";
import type { Sketch } from "schemas/database";
import { SplitView } from "components/pages/editor/splitView";
import { UserContext } from "contexts/user";
import { execute } from "actions/code/execute";
import { getSketch } from "database/index";
import { redirect } from "next/navigation";
import { saveSketch } from "actions/code/saveSketch";
import styles from "styles/pages/editor.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";
import { useStreamAction } from "hooks/useStreamAction";

/**
 * This is the editor page.
 * @returns The page element.
 */
// eslint-disable-next-line max-statements, max-lines-per-function
export default function EditorPage(): ReactNode {
    const { user } = useContext(UserContext);
    const { setType, setMessage } = useContext(NotificationsContext);
    const [openShare, setOpenShare] = useState(false);
    const [openOpen, setOpenOpen] = useState(false);
    const [openSave, setOpenSave] = useState(false);
    const [openOpenWarning, setOpenOpenWarning] = useState(false);
    const [openNewWarning, setOpenNewWarning] = useState(false);
    const [saved, setSaved, resetSaved] = useLocalStorage("saved", false);
    const [id, setId, resetId] = useLocalStorage<string | null>("id", null);
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
    const [author, setAuthor] = useLocalStorage<string | null>("author", user === null
        ? null
        : user.username ?? user.email.split("@")[0]!);
    const [codeOutput, executeStream, terminateStream, clearStream] = useStreamAction(execute);
    const [loaded, setLoaded] = useState(false);
    const [, loadDocker] = useStreamAction(execute);
    const [graphics, setGraphics] = useState<string[]>([]);

    const reset = (): void => {
        clearStream();
        resetTitle();
        resetCode();
        resetSaved();
        resetId();
        setAuthor(user?.username ?? user?.email.split("@")[0] ?? null);
        redirect("/editor");
    };

    const clear = clearStream;
    const new_ = (): void => {
        if (user === null || saved)
            reset();
        else
            setOpenNewWarning(true);
    };
    const open = (): void => {
        if (user === null || saved)
            setOpenOpen(true);
        else
            setOpenOpenWarning(true);
    };
    const save = (): void => {
        if (id === null) {
            setOpenSave(true);
        } else {
            const formData = new FormData();

            formData.append("id", id);
            formData.append("content", code);

            saveSketch(formData).then(() => {
                setSaved(true);
                setType("success");
                setMessage("Sketch saved.");
            }).catch((e: unknown) => {
                setType("error");
                setMessage(e instanceof Error ? e.message : "An error occurred.");
            });
        }
    };
    const share = (): void => setOpenShare(true);
    const stop = terminateStream;
    const run = (): void => {
        stop();
        executeStream(decompressFromEncodedURIComponent(code));
    };

    // Remove the graphics commands from the console output.
    const consoleOutput = codeOutput.join("")
        .split("\n")
        .map((output) => (output.startsWith("drawToCanvas(") ? "Compiling..." : output))
        .join("\n")
        .replace(/\n+/g, "\n");

    // Extract the graphics commands, and send them to the canvas.
    const newGraphics = codeOutput.join("").match(/drawToCanvas\((.*)\)/g) ?? [];

    if (JSON.stringify(graphics) !== JSON.stringify(newGraphics))
        setGraphics(newGraphics);

    // Handle URL parameters and key presses.
    useEffect(() => {
        const url = new URL(window.location.href);
        const idParam = url.searchParams.get("id");

        if (idParam === null) {
            const codeParam = url.searchParams.get("code");
            const titleParam = url.searchParams.get("title");
            const authorParam = url.searchParams.get("author");

            if (codeParam !== null)
                setCode(codeParam);

            if (titleParam !== null)
                setTitle(decodeURIComponent(titleParam));

            if (authorParam !== null)
                setAuthor(authorParam);
        } else if (user !== null) {
            getSketch(idParam).then((sketchJson) => {
                const sketchObj = JSON.parse(sketchJson) as Sketch;

                setTitle(sketchObj.name);
                setCode(sketchObj.content);
                setAuthor(user.username ?? user.email.split("@")[0] ?? null);
                setId(idParam);
                setSaved(true);
            }).catch(() => {
                setType("error");
                setMessage("Failed to open sketch. Are you logged in to the correct account?");
            });
        }
    }, [user]);

    // Loads the docker instance in case it's gone to sleep.
    useEffect(() => {
        if (!loaded) {
            loadDocker("main = putStrLn \"Hello, world!\"");
            setLoaded(true);
        }
    }, [loaded]);

    // This can't be done as an early return because React complains about rendering different numbers of hooks.
    if (useMediaQuery("(orientation: portrait)")) {
        return (
            <div className={styles.unavailable}>
                <Typography component="div" variant="h4">
                    The editor is not available in portrait.
                </Typography>
                <Typography>
                    Please put your device in landscape.
                    It's recommended to use a desktop or laptop for the best experience.
                </Typography>
            </div>
        );
    }

    return (
        <div className={`full-width ${styles.container}`}>
            <ShareMenu open={openShare} setOpen={setOpenShare} code={code} title={title} author={author} />
            <NewWarningMenu open={openNewWarning} setOpen={setOpenNewWarning} new={reset} save={save} />
            <OpenWarningMenu
                open={openOpenWarning} setOpen={setOpenOpenWarning} openFn={() => setOpenOpen(true)} save={save} />
            <OpenMenu open={openOpen} setOpen={setOpenOpen} />
            <SaveMenu open={openSave} setOpen={setOpenSave} code={code} setSaved={setSaved} setId={setId} />
            <Buttons
                new={new_} clear={clear} open={open} save={save} share={share} stop={stop} run={run}
                loggedIn={user !== null} author={author} title={title} saved={saved}
            />
            <SplitView id="editor-horizontal">
                <SplitView vertical id="editor-vertical">
                    <Editor
                        code={decompressFromEncodedURIComponent(code)} updateCode={updateCode}
                        save={save} open={open} new={new_} run={run} setSaved={setSaved}
                    />
                    <Console content={consoleOutput} />
                </SplitView>
                <Canvas content={graphics} />
            </SplitView>
        </div>
    );
}
