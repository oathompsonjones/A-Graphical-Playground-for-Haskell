import type { CanvasSchema, Shape } from "schemas/graphics";
import { ShapeType, canvasSchema, colours } from "schemas/graphics";
import { PlainPaper } from "./plainPaper";
import type { ReactNode } from "react";
import { memo } from "react";
import styles from "styles/components/pages/editor/canvas.module.css";

/**
 * This is the canvas which renders the output from the editor.
 * @param props - The properties of the component.
 * @param props.content - The content to render on the canvas.
 * @returns The canvas element.
 */
function CanvasComponent({ content }: { content: string[]; }): ReactNode {
    const timeouts: NodeJS.Timeout[] = [];

    const parseColour = (colour: number | string): string => {
        switch (typeof colour) {
            case "number": return colours[colour]!;
            default: return colour;
        }
    };

    const renderFrame = (frame: Shape[], context: CanvasRenderingContext2D): void => {
        // Draw each shape.
        for (const shape of frame.flat()) {
            context.beginPath();
            context.fillStyle = parseColour(shape.f);
            context.strokeStyle = parseColour(shape.s);
            context.lineWidth = shape.sw;

            switch (shape.t) {
                case ShapeType.Line:
                    context.moveTo(shape.p[0], shape.p[1]);
                    context.lineTo(shape.p[0] + shape.l * Math.cos(shape.a), shape.p[1] + shape.l * Math.sin(shape.a));
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
                            break;
                    }

                    break;
                case ShapeType.Arc:
                    if (!shape.o)
                        context.moveTo(shape.p[0], shape.p[1]);

                    context.ellipse(shape.p[0], shape.p[1], shape.h, shape.v, shape.a, shape.b, shape.e);
                    break;
            }

            context.closePath();
            context.fill();
            context.stroke();

            // Reset any transformations.
            context.resetTransform();
        }
    };

    const renderAnimation = (animation: CanvasSchema, context: CanvasRenderingContext2D): void => {
        for (let i = 0; i < animation.s.length; i++) {
            timeouts.push(setTimeout(() => {
                const frame = animation.s[i]!;

                // Reset the background.
                context.fillStyle = parseColour(animation.b);
                context.fillRect(0, 0, animation.w, animation.h);

                // Render the shapes.
                renderFrame(frame instanceof Array ? frame : [frame], context);
            }, animation.f === 0 ? 0 : i * 1000 / animation.f));
        }

        // Clear the canvas.
        context.clearRect(0, 0, context.canvas.width, context.canvas.height);

        // Render another loop of the animaiton once it finishes.
        if (animation.s.length > 1) {
            timeouts.push(setTimeout(() => {
                renderAnimation(animation, context);
            }, 1000 / animation.f * animation.s.length));
        }
    };

    const render = (canvas: HTMLCanvasElement | null): void => {
        timeouts.forEach(clearTimeout);

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
            canvas.width = animation.w;
            canvas.height = animation.h;

            // Set the background.
            context.fillStyle = parseColour(animation.b);
            context.fillRect(0, 0, animation.w, animation.h);

            // Render the animation.
            renderAnimation(animation, context);
        }
    };

    return (
        <PlainPaper className={styles.wrapper!}>
            <canvas id="canvas" width={500} height={500} ref={render} />
        </PlainPaper>
    );
}

export const Canvas = memo(CanvasComponent);
