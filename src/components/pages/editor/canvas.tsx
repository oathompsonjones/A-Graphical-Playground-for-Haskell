import type { CanvasSchema, Shape } from "schemas/graphics";
import { Connection, ShapeType, colours } from "schemas/graphics";
import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import { memo } from "react";
import styles from "styles/components/pages/editor/canvas.module.css";

/**
 * This is the canvas which renders the output from the editor.
 * @param props - The properties of the component.
 * @param props.canvas - The config for the canvas.
 * @param props.frame - The frame to render on the canvas.
 * @param props.index - The index of the current frame.
 * @param props.done - Whether the animation is done.
 * @returns The canvas element.
 */
function CanvasComponent({ canvas, frame, index }: {
    canvas: CanvasSchema | null;
    frame: Shape[];
    index: number;
    done: boolean;
}): ReactNode {
    const parseColour = (colour: number | string): string => {
        switch (typeof colour) {
            case "number": return colours[colour]!;
            default: return colour;
        }
    };

    const render = (element: HTMLCanvasElement | null): void => {
        if (element === null)
            return;

        const context = element.getContext("2d");

        if (context === null)
            return;

        requestAnimationFrame(() => {
            // Clear the canvas if this is the first frame.
            if (index === 0)
                context.clearRect(0, 0, element.width, element.height);

            // Set the canvas size.
            element.width = canvas?.w ?? 800;
            element.height = canvas?.h ?? 600;

            // Reset the background.
            context.fillStyle = parseColour(canvas?.b ?? "LightGrey");
            context.fillRect(0, 0, element.width, element.height);

            // Draw each shape.
            for (const shape of frame.flat()) {
                context.beginPath();
                context.fillStyle = parseColour(shape.f);
                context.strokeStyle = parseColour(shape.s);
                context.lineWidth = shape.sw;

                if (!("t" in shape))
                    continue;

                switch (shape.t) {
                    case ShapeType.Line:
                        context.moveTo(shape.p[0], shape.p[1]);
                        context.lineTo(
                            shape.p[0] + shape.l * Math.cos(shape.a),
                            shape.p[1] + shape.l * Math.sin(shape.a),
                        );
                        break;
                    case ShapeType.Ellipse:
                        context.ellipse(shape.p[0], shape.p[1], shape.h, shape.v, shape.a, 0, 2 * Math.PI);
                        break;
                    case ShapeType.Rect:
                    // Apply rotation about the position of the rectangle.
                        context.translate(shape.p[0], shape.p[1]);
                        context.rotate(shape.a);
                        context.translate(-shape.p[0], -shape.p[1]);

                        // Draw the rectangle.
                        context.rect(shape.p[0], shape.p[1], shape.w, shape.h);
                        break;
                    case ShapeType.Polygon:
                        if (shape.v.length === 0)
                            break;

                        // Apply rotation about the position of the polygon.
                        context.translate(shape.p[0], shape.p[1]);
                        context.rotate(shape.a);
                        context.translate(-shape.p[0], -shape.p[1]);

                        // Draw the polygon.
                        context.beginPath();
                        context.moveTo(shape.v[0]![0] + shape.p[0], shape.v[0]![1] + shape.p[1]);

                        for (const point of shape.v.slice(1))
                            context.lineTo(point[0] + shape.p[0], point[1] + shape.p[1]);

                        context.closePath();
                        break;
                    case ShapeType.Curve:
                        if (shape.v.length !== 2 && shape.v.length !== 3)
                            break;

                        // Apply rotation about the position of the polygon.
                        context.translate(shape.p[0], shape.p[1]);
                        context.rotate(shape.a);
                        context.translate(-shape.p[0], -shape.p[1]);

                        // Move to the start of the curve.
                        context.moveTo(shape.p[0], shape.p[1]);

                        // Draw the curve.
                        switch (shape.v.length) {
                            case 2:
                                context.quadraticCurveTo(
                                    shape.v[0]![0] + shape.p[0],
                                    shape.v[0]![1] + shape.p[1],
                                    shape.v[1]![0] + shape.p[0],
                                    shape.v[1]![1] + shape.p[1],
                                );
                                context.moveTo(shape.p[0], shape.p[1]);
                                break;
                            case 3:
                                context.bezierCurveTo(
                                    shape.v[0]![0] + shape.p[0],
                                    shape.v[0]![1] + shape.p[1],
                                    shape.v[1]![0] + shape.p[0],
                                    shape.v[1]![1] + shape.p[1],
                                    shape.v[2]![0] + shape.p[0],
                                    shape.v[2]![1] + shape.p[1],
                                );
                                context.moveTo(shape.p[0], shape.p[1]);
                                break;
                        }

                        break;
                    case ShapeType.Arc:
                        if (shape.c === Connection.Pie)
                            context.moveTo(shape.p[0], shape.p[1]);

                        context.ellipse(shape.p[0], shape.p[1], shape.h, shape.v, shape.a, shape.b, shape.e);

                        if (shape.c === Connection.Open)
                            context.moveTo(shape.p[0], shape.p[1]);

                        break;
                }

                context.closePath();
                context.fill();
                context.stroke();

                // Reset any transformations.
                context.resetTransform();
            }
        });
    };

    return (
        <PlainPaper className={styles.wrapper!}>
            <canvas id="canvas" width={800} height={600} ref={render} />
        </PlainPaper>
    );
}

export const Canvas = memo(CanvasComponent);
