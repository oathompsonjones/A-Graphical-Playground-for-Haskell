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
 * @param props.message - The message to display on the tooltip.
 * @returns The console element.
 */
function ConsoleComponent({ content, status, message }: {
    readonly content?: string;
    readonly status: "done" | "error" | "running";
    readonly message: string;
}): ReactNode {
    const colourMap = {
        done: "warning",
        error: "error",
        running: "success",
    } as const;

    const messageMap = {
        done: "Listener is idle",
        error: `Listener has encountered an error: ${message}`,
        running: "Listener is running",
    } as const;

    return (
        <PlainPaper className={styles.console}>
            <Tooltip arrow placement="left" title={messageMap[status]}>
                <Typography className={styles.indicator}>
                    <Circle color={colourMap[status]} />
                </Typography>
            </Tooltip>
            <pre>{content}</pre>
        </PlainPaper>
    );
}

export const Console = memo(ConsoleComponent);
