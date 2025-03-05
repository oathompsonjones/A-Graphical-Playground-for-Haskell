import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import { memo } from "react";
import styles from "styles/components/pages/editor/console.module.css";

/**
 * This is the console to display code outputs.
 * @param props - The properties of the component.
 * @param props.content - The content to display on the console.
 * @returns The console element.
 */
function ConsoleComponent({ content }: { content?: string; }): ReactNode {
    return (
        <PlainPaper className={styles.console!}>
            <pre>{content}</pre>
        </PlainPaper>
    );
}

export const Console = memo(ConsoleComponent);
