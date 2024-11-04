import { ButtonGroup, Divider, IconButton, Tooltip, Typography } from "@mui/material";
import { FileOpen, InsertDriveFile, IosShare, PlayArrow, Save, Stop } from "@mui/icons-material";
import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import styles from "styles/components/buttons.module.css";

/**
 * Contains the buttons to interact with the editor.
 * @param props - The properties of the component.
 * @param props.title - The name of the current sketch.
 * @param props.new - The function to create a new file.
 * @param props.open - The function to open a file.
 * @param props.run - The function to run the code.
 * @param props.save - The function to save the file.
 * @param props.share - The function to share the file.
 * @param props.stop - The function to stop the code.
 * @returns The buttons element.
 */
export function Buttons({ title, new: new_, open, run, save, share, stop }:
Record<"new" | "open" | "run" | "save" | "share" | "stop", () => void> & { title: string; }): ReactNode {
    return (
        <PlainPaper className={styles.container!}>
            <ButtonGroup variant="text">
                <Tooltip title="New" arrow>
                    <IconButton onClick={new_}>
                        <InsertDriveFile />
                    </IconButton>
                </Tooltip>
                <Tooltip title="Open" arrow>
                    <IconButton disabled onClick={open}>
                        <FileOpen />
                    </IconButton>
                </Tooltip>
                <Tooltip title="Save" arrow>
                    <IconButton disabled onClick={save}>
                        <Save />
                    </IconButton>
                </Tooltip>
            </ButtonGroup>
            <Divider orientation="vertical" className={styles.divider!} />
            <Typography className={styles.heading!}>
                {title}
            </Typography>
            <Divider orientation="vertical" className={styles.divider!} />
            <ButtonGroup variant="text">
                <Tooltip title="Share" arrow>
                    <IconButton onClick={share}>
                        <IosShare />
                    </IconButton>
                </Tooltip>
            </ButtonGroup>
            <Divider orientation="vertical" className={styles.divider!} />
            <ButtonGroup variant="text" className={styles.right!}>
                <Tooltip title="Stop" arrow>
                    <IconButton color="error" disabled onClick={stop}>
                        <Stop />
                    </IconButton>
                </Tooltip>
                <Tooltip title="Run" arrow>
                    <IconButton color="success" onClick={run}>
                        <PlayArrow />
                    </IconButton>
                </Tooltip>
            </ButtonGroup>
        </PlainPaper>
    );
}
