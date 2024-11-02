import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import styles from "styles/components/console.module.css";

/**
 * This is the console to display code outputs.
 * @param props - The properties of the component.
 * @param props.content - The content to display on the console.
 * @returns The console element.
 */
export function Console({ content }: { content?: string; }): ReactNode {
    return (
        <PlainPaper className={styles.console!}>
            <pre>{content}</pre>
        </PlainPaper>
    );
}
