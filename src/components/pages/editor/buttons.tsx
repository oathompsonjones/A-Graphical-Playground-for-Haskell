import { ButtonGroup, Divider, IconButton, Tooltip, Typography } from "@mui/material";
import { Clear, FileOpen, InsertDriveFile, IosShare, PlayArrow, Save, Stop } from "@mui/icons-material";
import { memo, useContext, useEffect, useState } from "react";
import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import { SketchContext } from "contexts/sketch";
import { UserContext } from "contexts/user";
import styles from "styles/components/pages/editor/buttons.module.css";

// TODO: Loop toggle?
// TODO: Pixelated toggle?

/**
 * Contains the buttons to interact with the editor.
 * @param props - The properties of the component.
 * @param props.clear - The function to clear the output.
 * @param props.new - The function to create a new file.
 * @param props.open - The function to open a file.
 * @param props.run - The function to run the code.
 * @param props.save - The function to save the file.
 * @param props.share - The function to share the file.
 * @param props.stop - The function to stop the code.
 * @returns The buttons element.
 */
function ButtonsComponent({ clear, new: new_, open, run, save, share, stop }:
Record<"clear" | "new" | "open" | "run" | "save" | "share" | "stop", () => void>): ReactNode {
    const { user } = useContext(UserContext);
    const loggedIn = user !== null;
    const { author, saved, title } = useContext(SketchContext);
    const [mounted, setMounted] = useState(false);

    let displayTitle: string | null = null;

    if (mounted)
        displayTitle = author === null ? title : `${author}/${title} — ${saved ? "Saved" : "Unsaved"}`;

    const accountButtons = loggedIn
        ? (
            <ButtonGroup variant="text">
                <Tooltip arrow title="Open">
                    <IconButton onClick={open}><FileOpen /></IconButton>
                </Tooltip>
                <Tooltip arrow title="Save">
                    <IconButton onClick={save}><Save /></IconButton>
                </Tooltip>
            </ButtonGroup>
        )
        : (
            <ButtonGroup disabled variant="text">
                <IconButton disabled><FileOpen /></IconButton>
                <IconButton disabled><Save /></IconButton>
            </ButtonGroup>
        );

    useEffect(() => setMounted(true), []);

    return (
        <PlainPaper className={styles.container}>
            <ButtonGroup variant="text">
                <Tooltip arrow title="New">
                    <IconButton onClick={new_}>
                        <InsertDriveFile />
                    </IconButton>
                </Tooltip>
            </ButtonGroup>

            <Divider className={styles.divider} orientation="vertical" />

            {accountButtons}

            <Divider className={styles.divider} orientation="vertical" />

            <ButtonGroup variant="text">
                <Tooltip arrow title="Share">
                    <IconButton onClick={share}>
                        <IosShare />
                    </IconButton>
                </Tooltip>
            </ButtonGroup>

            <Divider className={styles.divider} orientation="vertical" />

            <Typography className={styles.heading}>{displayTitle}</Typography>

            <Divider className={styles.divider} orientation="vertical" />

            <ButtonGroup className={styles.right} variant="text">
                <Tooltip arrow title="Clear Output">
                    <IconButton color="warning" onClick={clear}>
                        <Clear />
                    </IconButton>
                </Tooltip>
                <Tooltip arrow title="Stop">
                    <IconButton color="error" onClick={stop}>
                        <Stop />
                    </IconButton>
                </Tooltip>
                <Tooltip arrow title="Run">
                    <IconButton color="success" onClick={run}>
                        <PlayArrow />
                    </IconButton>
                </Tooltip>
            </ButtonGroup>
        </PlainPaper>
    );
}

export const Buttons = memo(ButtonsComponent);
