"use client";

import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import styles from "styles/components/canvas.module.css";
import { useEffect } from "react";

const defaultBackground = "rgb(255,255,255)";
const defaultStroke = "rgb(0,0,0)";
const defaultFill = "rgba(0,0,0,0)";
const defaultWeight = 2;
const parsePoint = (str: string): [number, number] => str.slice(1, -1).split(",").map(Number) as [number, number];
const parseColor = (str: string, def: string): string => (str === "Nothing" ? def : str.slice(5));
const parseWeight = (str: string, def: number): number => (str === "Nothing" ? def : Number(str));

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

        context.fillStyle = defaultBackground;
        context.fillRect(0, 0, canvas.width, canvas.height);

        context.strokeStyle = defaultFill;
        context.lineWidth = defaultWeight;

        const drawToCanvasRegex = /drawToCanvas\((([^(]*)\((.*))\)\)/;

        for (const item of content) {
            const input = drawToCanvasRegex.exec(item);

            if (input === null)
                continue;

            const [,, func, argsString] = input;
            const args = argsString!.split(", ");

            switch (func) {
                case "background": {
                    context.fillStyle = args[0]!;
                    context.fillRect(0, 0, canvas.width, canvas.height);
                    break;
                }
                case "circle": {
                    const [x, y] = parsePoint(args[0]!);
                    const r = Number(args[1]);

                    context.beginPath();
                    context.arc(x, y, r, 0, 2 * Math.PI);

                    context.fillStyle = parseColor(args[2]!, defaultFill);
                    context.strokeStyle = parseColor(args[3]!, defaultStroke);
                    context.lineWidth = parseWeight(args[4]!, defaultWeight);

                    break;
                }
                case "ellipse": {
                    const [x, y] = parsePoint(args[0]!);
                    const [w, h] = args.slice(1, 3).map(Number);

                    context.beginPath();
                    context.ellipse(x, y, w!, h!, 0, 0, 2 * Math.PI);

                    context.fillStyle = parseColor(args[3]!, defaultFill);
                    context.strokeStyle = parseColor(args[4]!, defaultStroke);
                    context.lineWidth = parseWeight(args[5]!, defaultWeight);

                    break;
                }
                case "line": {
                    const [x1, y1] = parsePoint(args[0]!);
                    const [x2, y2] = parsePoint(args[1]!);

                    context.beginPath();
                    context.moveTo(x1, y1);
                    context.lineTo(x2, y2);

                    context.fillStyle = parseColor(args[2]!, defaultFill);
                    context.strokeStyle = parseColor(args[3]!, defaultStroke);
                    context.lineWidth = parseWeight(args[4]!, defaultWeight);

                    break;
                }
                case "point": {
                    const [x, y] = parsePoint(args[0]!);

                    context.beginPath();
                    context.arc(x, y, 1, 0, 2 * Math.PI);

                    context.fillStyle = parseColor(args[1]!, defaultFill);
                    context.strokeStyle = parseColor(args[2]!, defaultStroke);
                    context.lineWidth = parseWeight(args[3]!, defaultWeight);

                    break;
                }
                case "quad": {
                    const [x1, y1] = parsePoint(args[0]!);
                    const [x2, y2] = parsePoint(args[1]!);
                    const [x3, y3] = parsePoint(args[2]!);
                    const [x4, y4] = parsePoint(args[3]!);

                    context.beginPath();
                    context.moveTo(x1, y1);
                    context.lineTo(x2, y2);
                    context.lineTo(x3, y3);
                    context.lineTo(x4, y4);
                    context.closePath();

                    console.log(args, parseColor(args[5]!, defaultStroke));

                    context.fillStyle = parseColor(args[4]!, defaultFill);
                    context.strokeStyle = parseColor(args[5]!, defaultStroke);
                    context.lineWidth = parseWeight(args[6]!, defaultWeight);

                    console.log(context.strokeStyle);

                    break;
                }
                case "rect": {
                    const [x, y] = parsePoint(args[0]!);
                    const [w, h] = args.slice(1, 3).map(Number);

                    context.rect(x, y, w!, h!);

                    context.fillStyle = parseColor(args[3]!, defaultFill);
                    context.strokeStyle = parseColor(args[4]!, defaultStroke);
                    context.lineWidth = parseWeight(args[5]!, defaultWeight);

                    break;
                }
                case "square": {
                    const [x, y] = parsePoint(args[0]!);
                    const s = Number(args[1]);

                    context.rect(x, y, s, s);

                    context.fillStyle = parseColor(args[2]!, defaultFill);
                    context.strokeStyle = parseColor(args[3]!, defaultStroke);
                    context.lineWidth = parseWeight(args[4]!, defaultWeight);

                    break;
                }
                case "triangle": {
                    const [x1, y1] = parsePoint(args[0]!);
                    const [x2, y2] = parsePoint(args[1]!);
                    const [x3, y3] = parsePoint(args[2]!);

                    context.beginPath();
                    context.moveTo(x1, y1);
                    context.lineTo(x2, y2);
                    context.lineTo(x3, y3);
                    context.closePath();

                    context.fillStyle = parseColor(args[3]!, defaultFill);
                    context.strokeStyle = parseColor(args[4]!, defaultStroke);
                    context.lineWidth = parseWeight(args[5]!, defaultWeight);

                    break;
                }
                default:
                    break;
            }

            context.fill();
            context.stroke();
        }
    }, [content]);

    return (
        <PlainPaper className={styles.wrapper!}>
            <canvas id="canvas" width={width ?? 500} height={height ?? 500} />
        </PlainPaper>
    );
}
