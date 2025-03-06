"use client";

import type { CanvasSchema, Shape } from "schemas/graphics";
import type { ReactNode, RefObject } from "react";
import { canvasSchema, shapeSchema } from "schemas/graphics";
import { memo, useRef, useState } from "react";
import { Canvas } from "./canvas";

/**
 * This is the canvas which renders the output from the editor.
 * @param props - The properties of the component.
 * @param props.content - The content to render on the canvas.
 * @param props.interval - The interval for the canvas.
 * @returns The canvas element.
 */
function CanvasControllerComponent({ content: [canvasInput, ...frameInputs], interval }: {
    content: string[];
    interval: RefObject<NodeJS.Timeout | null>;
}): ReactNode {
    const frameIndexRef = useRef(0);
    const framesLength = useRef(0);
    const done = useRef(false);
    const [frameIndex, setFrameIndex] = useState(frameIndexRef.current);

    if (isNaN(frameIndexRef.current))
        frameIndexRef.current = 0;

    if (isNaN(framesLength.current))
        framesLength.current = 0;

    // Parse the canvas config.
    let canvas: CanvasSchema | null = null;

    try {
        const extract = (/canvas\((.*)\)/).exec(canvasInput ?? "");

        if (extract === null || typeof extract[1] !== "string")
            throw new Error("Failed to extract canvas config.");

        const object = JSON.parse(extract[1]) as unknown;

        canvas = canvasSchema.parse(object) as unknown as CanvasSchema;
    } catch (error) {
        // Continue
    }

    // Parse the frames.
    const frames: Shape[] = [];

    for (const frameInput of frameInputs) {
        if (frameInput.startsWith("done()"))
            done.current = true;

        try {
            const extract = (/frame\((.*)\)/).exec(frameInput);

            if (extract === null || typeof extract[1] !== "string")
                throw new Error("Failed to extract frame.");

            const object = JSON.parse(extract[1]) as unknown;

            framesLength.current = frames.push(shapeSchema.parse(object) as unknown as Shape);
        } catch (error) {
            continue;
        }
    }

    const frame = frames[frameIndexRef.current] ?? [];

    if (canvas !== null && canvas.r > 0 && interval.current === null) {
        interval.current = setInterval(() => {
            frameIndexRef.current = done.current
                ? (frameIndexRef.current + 1) % framesLength.current
                : Math.min(frameIndexRef.current + 1, framesLength.current - 1);
            setFrameIndex(frameIndexRef.current);
        }, 1000 / canvas.r);
    }

    return (
        <Canvas
            canvas={canvas}
            frame={frame instanceof Array ? frame : [frame]}
            index={frameIndex}
            done={done.current}
        />
    );
}

export const CanvasController = memo(CanvasControllerComponent);
