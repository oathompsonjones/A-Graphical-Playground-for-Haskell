"use client";

import type { MouseEvent, ReactElement, ReactNode } from "react";
import { useEffect, useRef, useState } from "react";
import { DragHandleRounded } from "@mui/icons-material";
import styles from "styles/components/splitView.module.css";
import { useLocalStorage } from "hooks/useLocalStorage";

/**
 * Creates a resizable split view.
 * @param props - The properties of the split view.
 * @param props.children - The two elements to split.
 * @param props.id - The unique identifier of the split view.
 * @param props.vertical - Whether the split view is vertical.
 * @returns The split view element.
 */
export function SplitView({ children, id, vertical }: {
    children: [ReactElement, ReactElement];
    id: string;
    vertical?: boolean;
}): ReactNode {
    const container = useRef<HTMLDivElement>(null!);
    const [size, setSize] = useLocalStorage(`${id}-size`, NaN);
    const [canDrag, setCanDrag] = useState(false);
    const enableDrag = (): void => setCanDrag(true);
    const disableDrag = (): void => setCanDrag(false);

    const orientation = vertical ?? false ? "vertical" : "horizontal";
    const splitViewClass = `${styles.container} ${styles[orientation]}`;
    const clientSize = ({ horizontal: "clientX", vertical: "clientY" } as const)[orientation];
    const containerPosition = ({ horizontal: "left", vertical: "top" } as const)[orientation];
    const containerSize = ({ horizontal: "width", vertical: "height" } as const)[orientation];

    const updateSizes = (): void => {
        (container.current.children[0] as HTMLDivElement).style[containerSize] = `${size}%`;
        (container.current.children[2] as HTMLDivElement).style[containerSize] = `${100 - size}%`;
    };

    const handleDrag = (event: MouseEvent<HTMLDivElement>): void => {
        if (!canDrag)
            return;

        const containerRect = container.current.getBoundingClientRect();

        setSize((event[clientSize] - containerRect[containerPosition]) / containerRect[containerSize] * 100);
        updateSizes();
    };

    useEffect(updateSizes, []);

    return (
        <div
            className={splitViewClass} ref={container}
            onMouseMove={handleDrag} onMouseLeave={disableDrag} onMouseUp={disableDrag}>
            {children[0]}
            <div className={styles.splitter} onMouseDown={enableDrag}>
                <DragHandleRounded className={styles.dragIcon!} />
            </div>
            {children[1]}
        </div>
    );
}
