import { Canvas } from "components/pages/editor/canvas";
import { Console } from "components/pages/editor/console";
import { Editor } from "components/pages/editor/editor";
import type { Metadata } from "next";
import type { ReactNode } from "react";
import styles from "styles/pages/editor.module.css";

export const metadata: Metadata = { title: "A Graphical Playground for Haskell | Editor" };

/**
 * This is the editor page.
 * @returns The page element.
 */
export default function EditorPage(): ReactNode {
    return (
        <div className={`edge ${styles.container}`}>
            <div className={styles.column}>
                <Canvas />
            </div>
            <div className={styles.column}>
                <Editor />
                <Console />
            </div>
        </div>
    );
}
