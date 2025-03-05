"use client";

import { Typography, useMediaQuery } from "@mui/material";
import { useCallback, useContext, useEffect, useState } from "react";
import { Buttons } from "components/pages/editor/buttons";
import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import { NewWarningMenu } from "components/menus/newWarningMenu";
import { NotificationsContext } from "contexts/notifications";
import { OpenMenu } from "components/menus/openMenu";
import { OpenWarningMenu } from "components/menus/openWarningMenu";
import type { ReactNode } from "react";
import { SaveMenu } from "components/menus/saveMenu";
import { ShareMenu } from "components/menus/shareMenu";
import type { Sketch } from "schemas/database";
import { SketchContext } from "contexts/sketch";
import { SplitView } from "components/pages/editor/splitView";
import { UserContext } from "contexts/user";
import { decompressFromEncodedURIComponent } from "lz-string";
import { execute } from "actions/code/execute";
import { getSketch } from "database/index";
import { redirect } from "next/navigation";
import { saveSketch } from "actions/code/saveSketch";
import styles from "styles/pages/editor.module.css";
import { useStreamAction } from "hooks/useStreamAction";

/**
 * This is the editor page.
 * @returns The page element.
 */
// eslint-disable-next-line max-statements
export default function EditorPage(): ReactNode {
    const { user } = useContext(UserContext);
    const {
        code, id, resetCode, resetId, resetSaved, resetTitle,
        saved, setAuthor, setCode, setId, setSaved, setTitle,
    } = useContext(SketchContext);
    const { setNotification } = useContext(NotificationsContext);
    const [openShare, setOpenShare] = useState(false);
    const [openOpen, setOpenOpen] = useState(false);
    const [openSave, setOpenSave] = useState(false);
    const [openOpenWarning, setOpenOpenWarning] = useState(false);
    const [openNewWarning, setOpenNewWarning] = useState(false);
    const [codeOutput, executeStream, terminateStream, clearStream] = useStreamAction(execute);
    const [loaded, setLoaded] = useState(false);
    const [, loadDocker] = useStreamAction(execute);
    const [graphics, setGraphics] = useState<string[]>([]);

    const reset = useCallback((): void => {
        clearStream();
        resetTitle();
        resetCode();
        resetSaved();
        resetId();
        setAuthor(user?.username ?? user?.email.split("@")[0] ?? null);
        redirect("/editor");
    }, [user]);

    const clear = useCallback(clearStream, []);
    const new_ = useCallback((): void => {
        if (user === null || saved)
            reset();
        else
            setOpenNewWarning(true);
    }, [user, saved]);
    const open = useCallback((): void => {
        if (user === null)
            return;

        if (saved)
            setOpenOpen(true);
        else
            setOpenOpenWarning(true);
    }, [user, saved]);
    const save = useCallback((): void => {
        if (user === null)
            return;

        if (id === null) {
            setOpenSave(true);
        } else {
            const formData = new FormData();

            formData.append("id", id);
            formData.append("content", code);

            saveSketch(formData).then(() => {
                setSaved(true);
                setNotification("Sketch saved.", "success");
            }).catch((e: unknown) => {
                setNotification(e instanceof Error ? e.message : "An error occurred.", "error");
            });
        }
    }, [user, id, code]);
    const share = useCallback((): void => setOpenShare(true), []);
    const stop = useCallback(terminateStream, []);
    const run = useCallback((): void => {
        stop();
        executeStream(decompressFromEncodedURIComponent(code));
    }, [code]);

    // Extract the graphics commands, and send them to the canvas.
    const newGraphics = codeOutput.join("").match(/(canvas|frame)\((.*)\)\n/g) ?? [];

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
                setNotification("Failed to open sketch. Are you logged in to the correct account?", "error");
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
            <ShareMenu open={openShare} setOpen={setOpenShare} />
            <NewWarningMenu open={openNewWarning} setOpen={setOpenNewWarning} new={reset} save={save} />
            <OpenWarningMenu
                open={openOpenWarning} setOpen={setOpenOpenWarning} openFn={() => setOpenOpen(true)} save={save} />
            <OpenMenu open={openOpen} setOpen={setOpenOpen} />
            <SaveMenu open={openSave} setOpen={setOpenSave} />
            <Buttons new={new_} clear={clear} open={open} save={save} share={share} stop={stop} run={run} />
            <SplitView id="editor-horizontal">
                <SplitView vertical id="editor-vertical">
                    <Editor save={save} open={open} new={new_} run={run} />
                    <Console content={consoleOutput} />
                </SplitView>
                <Canvas content={graphics} />
            </SplitView>
        </div>
    );
}
