"use client";

import type { MouseEvent, ReactElement, ReactNode } from "react";
import { useRef, useState } from "react";
import { DragHandleRounded } from "@mui/icons-material";
import styles from "styles/components/splitView.module.css";

/**
 * Creates a resizable split view.
 * @param props - The properties of the split view.
 * @param props.className - Any additional CSS classes to apply.
 * @param props.children - The two elements to split.
 * @param props.vertical - Whether the split view is vertical.
 * @returns The split view element.
 */
export function SplitView({ className, children, vertical }: {
    className?: string;
    children: [ReactElement, ReactElement];
    vertical?: boolean;
}): ReactNode {
    const orientation = vertical ?? false ? "vertical" : "horizontal";
    const splitViewClass = `${styles.container} ${styles[orientation]} ${className}`;
    const container = useRef<HTMLDivElement>(null!);

    const [canDrag, setCanDrag] = useState(false);
    const enableDrag = (): void => setCanDrag(true);
    const disableDrag = (): void => setCanDrag(false);
    const handleDrag = (event: MouseEvent<HTMLDivElement>): void => {
        if (canDrag) {
            const firstSection = container.current.children[0] as HTMLDivElement;
            const secondSection = container.current.children[2] as HTMLDivElement;
            const containerRect = container.current.getBoundingClientRect();

            const clientSize = ({ horizontal: "clientX", vertical: "clientY" } as const)[orientation];
            const containerPosition = ({ horizontal: "left", vertical: "top" } as const)[orientation];
            const containerSize = ({ horizontal: "width", vertical: "height" } as const)[orientation];

            const offset = event[clientSize] - containerRect[containerPosition];
            const firstSize = offset / containerRect[containerSize] * 100;
            const secondSize = 100 - firstSize;

            firstSection.style[containerSize] = `${firstSize}%`;
            secondSection.style[containerSize] = `${secondSize}%`;
        }
    };

    return (
        <div className={splitViewClass} ref={container} onMouseMove={handleDrag} onMouseLeave={disableDrag}>
            {children[0]}
            <div className={styles.splitter} onMouseDown={enableDrag} onMouseUp={disableDrag}>
                <DragHandleRounded className={styles.dragIcon!} />
            </div>
            {children[1]}
        </div>
    );
}
