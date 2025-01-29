"use client";

import type { Frame } from "schemas/graphics";
import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import { frameSchema } from "schemas/graphics";
import styles from "styles/components/pages/editor/canvas.module.css";
import { useState } from "react";

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

    const render = (canvas: HTMLCanvasElement | null): void => {
        if (canvas === null)
            return;

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
                    case "line":
                        context.moveTo(shape.position.x, shape.position.y);
                        context.lineTo(
                            shape.position.x + shape.length * Math.cos(shape.angle),
                            shape.position.y + shape.length * Math.sin(shape.angle),
                        );
                        break;
                    case "ellipse":
                        context.ellipse(
                            shape.position.x,
                            shape.position.y,
                            shape.horizontalAxis,
                            shape.verticalAxis,
                            shape.angle,
                            0,
                            2 * Math.PI,
                        );
                        break;
                    case "rect":
                        // Apply rotation about the position of the rectangle.
                        context.translate(shape.position.x, shape.position.y);
                        context.rotate(shape.angle);
                        context.translate(-shape.position.x, -shape.position.y);

                        // Draw the rectangle.
                        context.rect(
                            shape.position.x - shape.width / 2,
                            shape.position.y - shape.height / 2,
                            shape.width,
                            shape.height,
                        );

                        // Reset the rotation.
                        context.setTransform(1, 0, 0, 1, 0, 0);
                        break;
                    case "polygon":
                        if (shape.points.length === 0)
                            break;

                        // Apply rotation about the position of the polygon.
                        context.translate(shape.position.x, shape.position.y);
                        context.rotate(shape.angle);
                        context.translate(-shape.position.x, -shape.position.y);

                        // Draw the polygon.
                        context.beginPath();
                        context.moveTo(shape.points[0]!.x + shape.position.x, shape.points[0]!.y + shape.position.y);

                        for (const point of shape.points.slice(1))
                            context.lineTo(point.x + shape.position.x, point.y + shape.position.y);

                        context.closePath();

                        // Reset the rotation.
                        context.setTransform(1, 0, 0, 1, 0, 0);
                        break;
                }

                context.fill();
                context.stroke();
                context.closePath();
            }
        }
    };

    return (
        <PlainPaper className={styles.wrapper!}>
            <canvas id="canvas" width={width} height={height} ref={render} />
        </PlainPaper>
    );
}
