import { ButtonGroup, Divider, Icon, IconButton, Tooltip, Typography } from "@mui/material";
import {
    Clear, FileOpen, InsertDriveFile, IosShare, KeyboardCommandKey,
    KeyboardControlKey, KeyboardReturn, PlayArrow, Save, Stop,
} from "@mui/icons-material";
import { useEffect, useState } from "react";
import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import styles from "styles/components/pages/editor/buttons.module.css";

// TODO: Fix font size on icons

/**
 * Contains the buttons to interact with the editor.
 * @param props - The properties of the component.
 * @param props.title - The name of the current sketch.
 * @param props.clear - The function to clear the output.
 * @param props.new - The function to create a new file.
 * @param props.open - The function to open a file.
 * @param props.run - The function to run the code.
 * @param props.save - The function to save the file.
 * @param props.share - The function to share the file.
 * @param props.stop - The function to stop the code.
 * @param props.loggedIn - Whether the user is logged in.
 * @param props.author - The author of the sketch.
 * @param props.saved - Whether the sketch has been saved.
 * @returns The buttons element.
 */
export function Buttons({ title, clear, new: new_, open, run, save, share, stop, loggedIn, author, saved }:
Record<"clear" | "new" | "open" | "run" | "save" | "share" | "stop", () => void> & {
    title: string;
    loggedIn: boolean;
    author: string | null;
    saved: boolean;
}): ReactNode {
    const [metaKey, setMetaKey] = useState(<KeyboardControlKey fontSize="small" />);

    useEffect(() => {
        if (navigator.platform.includes("Mac"))
            setMetaKey(<KeyboardCommandKey fontSize="small" />);
    }, []);

    return (
        <PlainPaper className={styles.container!}>
            <ButtonGroup variant="text">
                <Tooltip title={<>New ({metaKey}<Icon fontSize="small">N</Icon>)</>} arrow>
                    <IconButton onClick={new_}>
                        <InsertDriveFile />
                    </IconButton>
                </Tooltip>
            </ButtonGroup>

            <Divider orientation="vertical" className={styles.divider!} />

            {loggedIn
                ? (<ButtonGroup variant="text">
                    <Tooltip title={<>Open ({metaKey}<Icon fontSize="small">O</Icon>)</>} arrow>
                        <IconButton onClick={open}>
                            <FileOpen />
                        </IconButton>
                    </Tooltip>
                    <Tooltip title={<>Save ({metaKey}<Icon fontSize="small">S</Icon>)</>} arrow>
                        <IconButton onClick={save}>
                            <Save />
                        </IconButton>
                    </Tooltip>
                </ButtonGroup>)
                : (<ButtonGroup variant="text" disabled>
                    <IconButton disabled>
                        <FileOpen />
                    </IconButton>
                    <IconButton disabled>
                        <Save />
                    </IconButton>
                </ButtonGroup>)}

            <Divider orientation="vertical" className={styles.divider!} />

            <ButtonGroup variant="text">
                <Tooltip title="Share" arrow>
                    <IconButton onClick={share}>
                        <IosShare />
                    </IconButton>
                </Tooltip>
            </ButtonGroup>

            <Divider orientation="vertical" className={styles.divider!} />

            <Typography className={styles.heading!}>
                {author === null ? title : `${author}/${title} — ${saved ? "Saved" : "Unsaved"}`}
            </Typography>

            <Divider orientation="vertical" className={styles.divider!} />

            <ButtonGroup variant="text" className={styles.right!}>
                <Tooltip title="Clear Output" arrow>
                    <IconButton color="warning" onClick={clear}>
                        <Clear />
                    </IconButton>
                </Tooltip>
                <Tooltip title="Stop" arrow>
                    <IconButton color="error" onClick={stop}>
                        <Stop />
                    </IconButton>
                </Tooltip>
                <Tooltip title={<>Run ({metaKey}<KeyboardReturn fontSize="small" />)</>} arrow>
                    <IconButton color="success" onClick={run}>
                        <PlayArrow />
                    </IconButton>
                </Tooltip>
            </ButtonGroup>
        </PlainPaper>
    );
}
