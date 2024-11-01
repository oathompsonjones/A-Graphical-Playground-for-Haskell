"use client";

import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import type { ReactNode } from "react";
import { SplitView } from "components/splitView";
import styles from "styles/pages/editor.module.css";
import { useMediaQuery } from "@mui/material";

/**
 * This is the editor page.
 * @returns The page element.
 */
export default function EditorPage(): ReactNode {
    const isPortrait = useMediaQuery("(orientation: portrait)");

    return (
        <SplitView className={`full-width ${styles.container}`} vertical={isPortrait} id="editor-horizontal">
            <SplitView vertical id="editor-vertical">
                <Editor />
                <Console />
            </SplitView>
            <Canvas />
        </SplitView>
    );
}
