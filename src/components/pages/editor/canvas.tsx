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
 * @param props.content - The content to render on the canvas.
 * @returns The canvas element.
 */
export function Canvas({ height, width, content }: { height?: number; width?: number; content: string[]; }): ReactNode {
    // eslint-disable-next-line max-statements
    useEffect(() => {
        const canvas = document.getElementById("canvas") as HTMLCanvasElement;
        const context = canvas.getContext("2d")!;

        let stroke = true;
        let fill = true;

        context.fillStyle = "white";
        context.fillRect(0, 0, canvas.width, canvas.height);

        context.strokeStyle = "black";
        context.lineWidth = 2;

        for (const item of content) {
            const input = (/drawToCanvas\(((.+)\((.*)\))\)/).exec(item);

            if (input === null)
                continue;

            const [,,func] = input;
            const args = input[3]!.split(", ").map(Number);

            switch (func) {
                case "background": {
                    const [r, g, b] = args as [number, number, number];

                    context.fillStyle = `rgb(${r}, ${g}, ${b})`;
                    context.fillRect(0, 0, canvas.width, canvas.height);
                    break;
                }
                case "fill": {
                    const [r, g, b] = args as [number, number, number];

                    fill = true;
                    context.fillStyle = `rgb(${r}, ${g}, ${b})`;
                    break;
                }
                case "stroke": {
                    const [r, g, b] = args as [number, number, number];

                    stroke = true;
                    context.strokeStyle = `rgb(${r}, ${g}, ${b})`;
                    break;
                }
                case "strokeWeight": {
                    const [w] = args as [number];

                    context.lineWidth = w;
                    break;
                }
                case "noStroke":
                    stroke = false;
                    break;
                case "noFill":
                    fill = false;
                    break;
                case "circle": {
                    const [x, y, d] = args as [number, number, number];

                    context.beginPath();
                    context.arc(x, y, d / 2, 0, 2 * Math.PI);

                    if (stroke)
                        context.stroke();

                    if (fill)
                        context.fill();

                    break;
                }
                case "ellipse": {
                    const [x, y, w, h] = args as [number, number, number, number];

                    context.beginPath();
                    context.ellipse(x, y, w / 2, h / 2, 0, 0, 2 * Math.PI);

                    if (stroke)
                        context.stroke();

                    if (fill)
                        context.fill();

                    break;
                }
                case "line": {
                    const [x, y, w, h] = args as [number, number, number, number];

                    context.beginPath();
                    context.moveTo(x, y);
                    context.lineTo(w, h);

                    if (stroke)
                        context.stroke();

                    break;
                }
                case "point": {
                    const [x, y] = args as [number, number];

                    context.beginPath();
                    context.arc(x, y, 1, 0, 2 * Math.PI);

                    if (stroke)
                        context.stroke();

                    break;
                }
                case "quad": {
                    // eslint-disable-next-line max-len
                    const [x1, y1, x2, y2, x3, y3, x4, y4] = args as [number, number, number, number, number, number, number, number];

                    context.beginPath();
                    context.moveTo(x1, y1);
                    context.lineTo(x2, y2);
                    context.lineTo(x3, y3);
                    context.lineTo(x4, y4);
                    context.closePath();

                    if (stroke)
                        context.stroke();

                    if (fill)
                        context.fill();

                    break;
                }
                case "rect": {
                    const [x, y, w, h] = args as [number, number, number, number];

                    context.rect(x, y, w, h);

                    if (stroke)
                        context.stroke();

                    if (fill)
                        context.fill();

                    break;
                }
                case "square": {
                    const [x, y, s] = args as [number, number, number];

                    context.rect(x, y, s, s);

                    if (stroke)
                        context.stroke();

                    if (fill)
                        context.fill();

                    break;
                }
                case "triangle": {
                    const [x1, y1, x2, y2, x3, y3] = args as [number, number, number, number, number, number];

                    context.beginPath();
                    context.moveTo(x1, y1);
                    context.lineTo(x2, y2);
                    context.lineTo(x3, y3);
                    context.closePath();

                    if (stroke)
                        context.stroke();

                    if (fill)
                        context.fill();

                    break;
                }
                default:
                    break;
            }
        }
    }, [content]);

    return (
        <PlainPaper className={styles.wrapper!}>
            <canvas id="canvas" width={width ?? 500} height={height ?? 500} />
        </PlainPaper>
    );
}
