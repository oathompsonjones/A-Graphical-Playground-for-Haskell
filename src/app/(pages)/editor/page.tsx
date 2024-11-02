"use client";

import { ButtonGroup, IconButton, Tooltip, useMediaQuery } from "@mui/material";
import { PlayArrow, Save, Stop } from "@mui/icons-material";
import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import { PlainPaper } from "components/pages/editor/plainPaper";
import type { ReactNode } from "react";
import { SplitView } from "components/splitView";
import { execute } from "actions/code/execute";
import styles from "styles/pages/editor.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";

/**
 * This is the editor page.
 * @returns The page element.
 */
export default function EditorPage(): ReactNode {
    const isPortrait = useMediaQuery("(orientation: portrait)");
    const [code, setCode] = useLocalStorage("code", "-- Start writing your code here.\n\n");
    const [consoleOutput, setConsoleOutput] = useLocalStorage("console-output", "");
    const run = async (): Promise<void> => setConsoleOutput(await execute(code));

    return (
        <div className={`full-width ${styles.container}`}>
            <PlainPaper className={styles.buttons!}>
                <ButtonGroup variant="text">
                    <Tooltip title="Save" arrow>
                        <IconButton disabled>
                            <Save />
                        </IconButton>
                    </Tooltip>
                    <Tooltip title="Stop" arrow>
                        <IconButton color="error" disabled>
                            <Stop />
                        </IconButton>
                    </Tooltip>
                    <Tooltip title="Run" arrow>
                        <IconButton color="success" onClick={run as () => void}>
                            <PlayArrow />
                        </IconButton>
                    </Tooltip>
                </ButtonGroup>
            </PlainPaper>
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
