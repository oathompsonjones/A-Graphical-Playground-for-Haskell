"use client";

import { useEffect, useState } from "react";
import type { Frame } from "schemas/graphics";
import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import { frameSchema } from "schemas/graphics";
import styles from "styles/components/canvas.module.css";

/**
 * This is the canvas which renders the output from the editor.
 * @param props - The properties of the component.
 * @param props.content - The content to render on the canvas.
 * @returns The canvas element.
 */
export function Canvas({ content }: { content: string[]; }): ReactNode {
    const drawToCanvasRegex = /drawToCanvas\((.*)\)/;

    const [width, setWidth] = useState(500);
    const [height, setHeight] = useState(500);

    useEffect(() => {
        const canvas = document.getElementById("canvas") as HTMLCanvasElement;
        const context = canvas.getContext("2d")!;

        // Reset the canvas.
        context.fillStyle = "white";
        context.fillRect(0, 0, canvas.width, canvas.height);

        for (const item of content) {
            // Extract the input.
            const input = drawToCanvasRegex.exec(item);

            if (input === null || typeof input[1] !== "string")
                continue;

            // Parse the input.
            let frame: Frame;

            try {
                frame = frameSchema.parse(JSON.parse(input[1]));
            } catch (err) {
                continue;
            }

            // Set the canvas size.
            setWidth(frame.width);
            setHeight(frame.height);

            // Set the background.
            context.fillStyle = frame.backgroundColor;
            context.fillRect(0, 0, canvas.width, canvas.height);

            // Draw each shape.
            for (const shape of frame.shapes.flat()) {
                context.beginPath();
                context.fillStyle = shape.fill;
                context.strokeStyle = shape.stroke;
                context.lineWidth = shape.strokeWeight;

                switch (shape.type) {
                    case "ellipse":
                        context.ellipse(
                            shape.position.x,
                            shape.position.y,
                            shape.horizontalAxis,
                            shape.verticalAxis,
                            0,
                            0,
                            2 * Math.PI,
                        );
                        break;
                    case "polygon":
                        context.beginPath();
                        context.moveTo(shape.points[0]!.x, shape.points[0]!.y);

                        for (const point of shape.points.slice(1))
                            context.lineTo(point.x, point.y);

                        context.closePath();
                        break;
                }

                context.fill();
                context.stroke();
                context.closePath();
            }
        }
    }, [content]);

    return (
        <PlainPaper className={styles.wrapper!}>
            <canvas id="canvas" width={width} height={height} />
        </PlainPaper>
    );
}
