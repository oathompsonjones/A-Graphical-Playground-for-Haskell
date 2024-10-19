"use client";

import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import styles from "styles/components/canvas.module.css";
import { useEffect } from "react";

/**
 * This is the canvas which renders the output from the editor.
 * @param props - The properties of the component.
 * @param props.height - The height of the canvas. Defaults to 500px.
 * @param props.width - The width of the canvas. Defaults to 500px.
 * @returns The canvas element.
 */
export function Canvas({ height, width }: { height?: number; width?: number; }): ReactNode {
    useEffect(() => {
        const canvas = document.getElementById("canvas") as HTMLCanvasElement;
        const context = canvas.getContext("2d")!;

        context.fillStyle = "white";
        context.fillRect(0, 0, canvas.width, canvas.height);
    }, []);

    return (
        <PlainPaper className={styles.wrapper!}>
            <canvas id="canvas" width={width ?? 500} height={height ?? 500} />
        </PlainPaper>
    );
}
