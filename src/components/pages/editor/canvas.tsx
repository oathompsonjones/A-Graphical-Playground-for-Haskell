"use client";

import type { CanvasSchema, Shape } from "schemas/graphics";
import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import { canvasSchema } from "schemas/graphics";
import { memo } from "react";
import styles from "styles/components/pages/editor/canvas.module.css";

/**
 * This is the canvas which renders the output from the editor.
 * @param props - The properties of the component.
 * @param props.content - The content to render on the canvas.
 * @returns The canvas element.
 */
function CanvasComponent({ content }: { content: string[]; }): ReactNode {
    const renderFrame = (frame: Shape[], context: CanvasRenderingContext2D): void => {
        // Draw each shape.
        for (const shape of frame.flat()) {
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
                        shape.position.x,
                        shape.position.y,
                        shape.width,
                        shape.height,
                    );
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
                    break;
            }

            context.fill();
            context.stroke();
            context.closePath();

            // Reset any transformations.
            context.resetTransform();
        }
    };

    const render = (canvas: HTMLCanvasElement | null): void => {
        if (canvas === null)
            return;

        const context = canvas.getContext("2d")!;

        // Reset the canvas.
        context.fillStyle = "white";
        context.fillRect(0, 0, canvas.width, canvas.height);

        for (const item of content) {
            // Extract the input.
            const input = (/drawToCanvas\((.*)\)/).exec(item);

            if (input === null || typeof input[1] !== "string")
                continue;

            // Parse the input.
            let animation: CanvasSchema;

            try {
                animation = canvasSchema.parse(JSON.parse(input[1])) as unknown as CanvasSchema;
            } catch (error) {
                continue;
            }

            // Update the canvas size.
            canvas.width = animation.width;
            canvas.height = animation.height;

            // Set the background.
            context.fillStyle = animation.backgroundColor;
            context.fillRect(0, 0, animation.width, animation.height);

            // Render the frame.
            const fps = 2;

            for (let j = 0; j < animation.shapes.length; j++) {
                const shape = animation.shapes[j]!;

                setTimeout(() => {
                    // Reset the background.
                    context.fillStyle = animation.backgroundColor;
                    context.fillRect(0, 0, animation.width, animation.height);

                    // Render the shapes.
                    renderFrame(shape instanceof Array ? shape : [shape], context);
                }, j * 1000 / fps);
            }
        }
    };

    return (
        <PlainPaper className={styles.wrapper!}>
            <canvas id="canvas" width={500} height={500} ref={render} />
        </PlainPaper>
    );
}

export const Canvas = memo(CanvasComponent);
