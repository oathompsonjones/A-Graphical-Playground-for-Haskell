import { Tooltip, Typography } from "@mui/material";
import { Circle } from "@mui/icons-material";
import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import { memo } from "react";
import styles from "styles/components/pages/editor/console.module.css";

/**
 * This is the console to display code outputs.
 * @param props - The properties of the component.
 * @param props.content - The content to display on the console.
 * @param props.status - The status to set the colour indicator.
 * @returns The console element.
 */
function ConsoleComponent({ content, status }: { content?: string; status: "done" | "error" | "running"; }): ReactNode {
    const colourMap = {
        done: "warning",
        error: "error",
        running: "success",
    } as const;

    const messageMap = {
        done: "Listener is idle",
        error: "Listener has encountered an error",
        running: "Listener is running",
    } as const;

    return (
        <PlainPaper className={styles.console!}>
            <Tooltip title={messageMap[status]} placement="left" arrow>
                <Typography className={styles.indicator!}>
                    <Circle color={colourMap[status]} />
                </Typography>
            </Tooltip>
            <pre>{content}</pre>
        </PlainPaper>
    );
}

export const Console = memo(ConsoleComponent);
